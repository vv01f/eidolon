--  eidolon -- A simple gallery in Haskell and Yesod
--  Copyright (C) 2015  Amedeo Molnár
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Affero General Public License as published
--  by the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Affero General Public License for more details.
--
--  You should have received a copy of the GNU Affero General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Handler.Commons where

import Import
import Scale
import qualified Data.Text as T
import Data.String
import qualified Data.List as L
import Data.Time
import Data.Maybe (fromJust)
import Data.Int (Int64(..))
import System.FilePath as FP
import System.Directory
import Text.Markdown
import Codec.Picture as P
import Codec.Picture.Metadata as PM hiding (insert, delete)
import Codec.Picture.ScaleDCT
import Codec.ImageType
import Graphics.Svg
import Graphics.Rasterific.Svg
import Graphics.Text.TrueType

import Debug.Trace

loginIsAdmin :: IsString t => Handler (Either (t, Route App) ())
loginIsAdmin = do
  musername <- maybeAuthId
  case musername of
    Just username -> do
      (Just (Entity userId user)) <- runDB $ getBy $ UniqueUser username
      if
        userAdmin user
        then
          return $ Right ()
        else
          return $ Left ("You have no admin rights", HomeR)
    Nothing ->
      return $ Left ("You are not logged in", AuthR LoginR)

profileCheck :: IsString t => UserId -> Handler (Either (t, Route App) User)
profileCheck userId = do
  tempUser <- runDB $ get userId
  case tempUser of
    Just user -> do
      musername <- maybeAuthId
      case musername of
        Just username -> do
          (Just (Entity loginId _)) <- runDB $ getBy $ UniqueUser username
          if
            loginId == userId
            then
              return $ Right user
            else
              return $ Left ("You can only change your own profile settings", UserR $ userName user)
        Nothing ->
          return $ Left ("You nedd to be logged in to change settings", AuthR LoginR)
    Nothing ->
      return $ Left ("This user does not exist", HomeR)

mediumCheck :: IsString t => MediumId -> Handler (Either (t, Route App) Medium)
mediumCheck mediumId = do
  tempMedium <- runDB $ get mediumId
  case tempMedium of
    Just medium -> do
      let ownerId = mediumOwner medium
      musername <- maybeAuthId
      case musername of
        Just username -> do
          (Just (Entity userId _)) <- runDB $ getBy $ UniqueUser username
          album <- runDB $ getJust $ mediumAlbum medium
          let presence = userId == ownerId
              albumOwnerPresence = userId == albumOwner album
          if
            presence || albumOwnerPresence
            then
              return $ Right medium
            else
              return $ Left ("You must own this medium to change its settings", MediumR mediumId)
        Nothing ->
          return $ Left ("You must be logged in to change settings", AuthR LoginR)
    Nothing ->
      return $ Left ("This medium does not exist", HomeR)

insertMedium :: Medium -> AlbumId -> Handler ()
insertMedium medium aId = do
  mId <- runDB $ insert medium
  inAlbum <- runDB $ getJust aId
  let newMediaList = mId : albumContent inAlbum
  runDB $ update aId [AlbumContent =. newMediaList]

deleteMedium :: MediumId -> Medium -> Handler ()
deleteMedium mId medium = do
  commEnts <- runDB $ selectList [CommentOrigin ==. mId] []
  -- delete comments first
  mapM_ (runDB . delete . entityKey) commEnts
  -- remove reference
  removeReference mId $ mediumAlbum medium
  -- delete Files
  mapM_ (liftIO . removeFile . normalise . L.tail)
    [ mediumPath medium
    , mediumThumb medium
    , mediumPreview medium
    ]
  -- delete database entry
  runDB $ delete mId

moveMedium :: Medium -> MediumId -> AlbumId -> Handler ()
moveMedium med mId destId = do
  $(logError) "getting destination"
  dest <- runDB $ getJust destId
  -- remove reference
  $(logError) "removing reference"
  removeReference mId $ mediumAlbum med
  -- move physical Files
  -- let filen = show $ length (albumContent dest) + 1
  let ac = albumContent dest
      [PersistInt64 int] = if L.null ac then [PersistInt64 1] else keyToValues $ maximum $ ac
      filen = show $ fromIntegral int + 1
      ext   = takeExtension $ mediumPath med
      prefix = "static" </> "data" </> T.unpack (extractKey $ albumOwner dest) </> T.unpack (extractKey destId)
      nPath = prefix </> filen ++ ext
      nThumb = prefix </> takeBaseName nPath ++ "_thumb.jpg"
      nPrev = prefix </> takeBaseName nPath ++ "_preview.jpg"
  $(logError) $ T.pack $ "copyFile" ++ nPath
  liftIO $ copyFile (L.tail $ mediumPath med) nPath
  liftIO $ copyFile (L.tail $ mediumThumb med) nThumb
  liftIO $ copyFile (L.tail $ mediumPreview med) nPrev
  -- remove physical files
  $(logError) "removeFile"
  mapM_ (liftIO . removeFile . normalise . L.tail)
    [ mediumPath med
    , mediumThumb med
    , mediumPreview med
    ]
  -- chenge filenames in database
  runDB $ update mId
    [ MediumPath =. '/' : nPath
    , MediumThumb =. '/' : nThumb
    , MediumPreview =. '/' : nPrev
    , MediumAlbum =. destId
    ]
  -- create new references
  let newMediaList = mId : albumContent dest
  runDB $ update destId [AlbumContent =. newMediaList]

removeReference :: MediumId -> AlbumId -> Handler ()
removeReference mId aId = do
  -- delete references next
  album <- runDB $ getJust aId
  let newMediaList = removeItem mId $ albumContent album
  -- update reference list
  runDB $ update aId [AlbumContent =. newMediaList]

data UploadSpec
  = NewFile
  | Replace MediumId

-- | function to handle uploaded media files
handleUpload
  :: Int                    -- ^ number of uploaded media
  -> AlbumId                -- ^ 'AlbumId' of destination album
  -> T.Text                 -- ^ Title or title prefix
  -> UTCTime                -- ^ Time of upload
  -> UserId                 -- ^ 'UserId' of media owner
  -> Maybe Markdown         -- ^ Description text
  -> [T.Text]               -- ^ Tags
  -> Int                    -- ^ Licence
  -> UploadSpec             -- ^ New file or replacement file?
  -> (Int, FileInfo)        -- ^ actual file
  -> Handler (Maybe T.Text) -- ^ Returns the filename if something fails
handleUpload len albumId prefix time owner desc tags licence spec (index, file) = do
  let mime = fileContentType file
  if mime `elem` acceptedTypes
    then do
      albRef <- runDB $ getJust albumId
      let ownerId = albumOwner albRef
      path <- writeOnDrive file ownerId albumId spec
      isOk <- liftIO $ checkCVE_2016_3714 path mime
      if isOk
        then do
          meta <- generateThumbs path ownerId albumId mime
          tempName <- if len == 1
            then return prefix
            else return
              ( prefix `T.append` " " `T.append` T.pack (show index) `T.append`
              " of " `T.append` T.pack (show len))
          case spec of
            NewFile -> do
              medium <- return $ Medium
                tempName
                ('/' : path)
                ('/' : metaThumbPath meta)
                mime
                time
                owner
                desc
                tags
                albumId
                ('/' : metaPreviewPath meta)
                licence
              insertMedium medium albumId
            Replace mId ->
              runDB $ update mId
                [ MediumPath =. ('/' : path)
                ]
          return Nothing
        else do
          liftIO $ removeFile (FP.normalise path)
          return $ Just $ fileName file
    else
      return $ Just $ fileName file

data ThumbsMeta = ThumbsMeta
  { metaThumbPath :: FP.FilePath
  , metaPreviewPath :: FP.FilePath
  }

-- | generate thumbnail and preview images from uploaded image
generateThumbs
  :: FP.FilePath        -- ^ Path to original image
  -> UserId             -- ^ Uploading user
  -> AlbumId            -- ^ Destination album
  -> T.Text             -- ^ MIME-Type (used for svg et al.)
  -> Handler ThumbsMeta -- ^ Resulting metadata to store
generateThumbs path uId aId mime = do
  orig <- case mime of
    "image/svg+xml" -> do
      svg <- liftIO $ loadSvgFile path
      liftIO $ traceIO "------------------> SVG loaded!"
      let (swidth, sheight) = documentSize 100 (fromJust svg)
          scale =
            let
              picScale = (fromIntegral swidth / fromIntegral sheight) :: Double
              nwidth =
                if swidth < 1000
                then 1000 :: Int
                else swidth
              nheight =
                if swidth < 1000
                then floor (1000 / picScale)
                else sheight
            in
              Just (nwidth, nheight)
      (img, _) <- liftIO $ renderSvgDocument emptyFontCache scale 100 $ fromJust svg
      liftIO $ traceIO "------------------> SVG rendered!"
      return img
    _ -> do
      eimg <- liftIO $ readImage path
      case eimg of
        Left err ->
          error err
        Right img -> -- This branch contains "classical" image formats like bmp or png
          return $ convertRGBA8 img
  let thumbName = FP.takeBaseName path ++ "_thumb.png"
      prevName = FP.takeBaseName path ++ "_preview.png"
      pathPrefix = "static" FP.</> "data" FP.</> T.unpack (extractKey uId) FP.</> T.unpack (extractKey aId)
      tPath = pathPrefix FP.</> thumbName
      pPath = pathPrefix FP.</> prevName
      -- origPix = convertRGBA8 orig
      oWidth = P.imageWidth orig :: Int
      oHeight = P.imageHeight orig :: Int
      tHeight = 230 :: Int
      pHeight = 600 :: Int
      tWidth = ceiling (fromIntegral oWidth / fromIntegral oHeight * fromIntegral tHeight :: Double)
      pScale = (fromIntegral pHeight :: Double) / (fromIntegral oHeight :: Double)
      pWidth = ceiling (fromIntegral oWidth * pScale)
      tPix = scaleBilinearAlpha tWidth tHeight orig
      pPix = scaleBilinearAlpha pWidth pHeight orig
  liftIO $ traceIO $ show oWidth
  liftIO $ traceIO "------------------> Image scaled!"
  liftIO $ savePngImage tPath $ ImageRGBA8 tPix
  liftIO $ traceIO "------------------> Saved thumbnail!"
  liftIO $ savePngImage pPath $ ImageRGBA8 pPix
  liftIO $ traceIO "------------------> Saved preview!"
  return $ ThumbsMeta
    { metaThumbPath    = tPath
    , metaPreviewPath  = pPath
    }

checkCVE_2016_3714 :: FP.FilePath -> T.Text -> IO Bool
checkCVE_2016_3714 p m =
  case m of
    "image/jpeg"     -> isJpeg p
    "image/jpg"      -> isJpeg p
    "image/png"      -> isPng p
    "image/x-ms-bmp" -> isBmp p
    "image/x-bmp"    -> isBmp p
    "image/bmp"      -> isBmp p
    "image/tiff"     -> isTiff p
    "image/tiff-fx"  -> isTiff p
    "image/svg+xml"  -> return True -- TODO: have to check XML for that.
    "image/gif"      -> isGif p
    _                -> return False

writeOnDrive :: FileInfo -> UserId -> AlbumId -> UploadSpec -> Handler FP.FilePath
writeOnDrive fil userId albumId spec = do
  album <- runDB $ getJust albumId
  let ac = albumContent album
  [PersistInt64 int] <- case spec of
    NewFile ->
      if L.null ac then return [PersistInt64 1] else return $ keyToValues $ maximum $ ac
    Replace mId -> do
      medium <- runDB $ getJust mId
      return $ (PersistInt64 (read $ takeBaseName $ mediumPath medium :: Int64)) : []
  let filen = show $ fromIntegral int + case spec of
        Replace _ -> 0
        NewFile   -> 1
      ext = FP.takeExtension $ T.unpack $ fileName fil
      path = "static" FP.</> "data" FP.</> T.unpack (extractKey userId) FP.</> T.unpack (extractKey albumId) FP.</> filen ++ ext
  dde <- liftIO $ doesDirectoryExist $ FP.dropFileName path
  if not dde
    then
      liftIO $ createDirectoryIfMissing True $ FP.dropFileName path
    else
      return ()
  liftIO $ fileMove fil path
  return path
