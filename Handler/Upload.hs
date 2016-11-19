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

module Handler.Upload where

import Import as I
import Handler.Commons
import Data.Time
import Data.Maybe
import qualified Data.Text as T
import Data.List as L
import qualified System.FilePath as FP
import System.Directory
import Text.Blaze.Internal
import Codec.ImageType
import Codec.Picture as P
import Codec.Picture.Metadata as PM
import Codec.Picture.ScaleDCT
import Graphics.Rasterific.Svg as SVG
import Graphics.Svg
import Graphics.Text.TrueType

getDirectUploadR :: AlbumId -> Handler Html
getDirectUploadR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of -- does the requested album exist
    Just album -> do
      let ownerId = albumOwner album
      msu <- lookupSession "userId"
      case msu of -- is anybody logged in
        Just tempUserId -> do
          let userId = getUserIdFromText tempUserId
          if
            userId == ownerId || userId `elem` albumShares album
            -- is the owner present or a user with whom the album is shared
            then do
              (dUploadWidget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ dUploadForm userId albumId
              defaultLayout $ do
                setTitle $ toHtml ("Eidolon :: Upload medium to " `T.append` albumTitle album)
                $(widgetFile "dUpload")
            else do
              setMessage "You must own this album to upload"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to upload"
          redirect LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect HomeR

postDirectUploadR :: AlbumId -> Handler Html
postDirectUploadR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of -- does the album exist
    Just album -> do
      let ownerId = albumOwner album
      msu <- lookupSession "userId"
      case msu of -- is anybody logged in
        Just tempUserId -> do
          let userId = getUserIdFromText tempUserId
          if userId == ownerId || userId `elem` albumShares album
            -- is the logged in user the owner or is the album shared with him
            then do
              ((result, _), _) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ dUploadForm userId albumId
              case result of
                FormSuccess temp -> do
                  let fils = fileBulkFiles temp
                  let indFils = zip [1..] fils
                  errNames <- mapM
                    (\(index, file) -> do
                      let mime = fileContentType file
                      if mime `elem` acceptedTypes
                        then do
                          path <- writeOnDrive file ownerId albumId
                          isOk <- liftIO $ checkCVE_2016_3714 path mime
                          if isOk
                            then do
                              meta <- generateThumbs path ownerId albumId
                              tempName <- if length indFils == 1
                                then return $ fileBulkPrefix temp
                                else return (fileBulkPrefix temp `T.append` " " `T.append` T.pack (show (index :: Int)) `T.append` " of " `T.append` T.pack (show (length indFils)))
                              let medium = Medium tempName ('/' : path) ('/' : metaThumbPath meta) mime (fileBulkTime temp) (fileBulkOwner temp) (fileBulkDesc temp) (fileBulkTags temp) albumId ('/' : metaPreviewPath meta)
                              -- mId <- runDB $ I.insert medium
                              -- inALbum <- runDB $ getJust albumId
                              -- let newMediaList = mId : albumContent inALbum
                              -- runDB $ update albumId [AlbumContent =. newMediaList]
                              insertMedium medium albumId
                              return Nothing
                          else do
                            liftIO $ removeFile (FP.normalise path)
                            return $ Just $ fileName file
                        else
                          return $ Just $ fileName file
                      ) indFils
                  let onlyErrNames = removeItem Nothing errNames
                  if
                    L.null onlyErrNames
                    then do
                      setMessage "All images succesfully uploaded"
                      redirect HomeR
                    else do
                      let justErrNames = map fromJust onlyErrNames
                      let msg = Content $ Text.Blaze.Internal.Text $ "File type not supported of: " `T.append` T.intercalate ", " justErrNames
                      setMessage msg
                      redirect HomeR
                _ -> do
                  setMessage "There was an error uploading the file"
                  redirect $ DirectUploadR albumId
            else do -- owner is not present
              setMessage "You must own this album to upload"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to upload"
          redirect $ AlbumR albumId
    Nothing -> do
      setMessage "This Album does not exist"
      redirect $ AlbumR albumId

-- | Type to pass metadata about imgaes around.
data ThumbsMeta = ThumbsMeta
  { metaThumbPath    :: FP.FilePath -- ^ Filepath of the new thumbnail image
  , metaPreviewPath  :: FP.FilePath -- ^ Filepath of the new preview image
  }

-- | generate thumbnail and preview images from uploaded image
generateThumbs
  :: FP.FilePath        -- ^ Path to original image
  -> UserId             -- ^ Uploading user
  -> AlbumId            -- ^ Destination album
  -> Handler ThumbsMeta -- ^ Resulting metadata to store
generateThumbs path uId aId = do
  eimg <- liftIO $ readImageWithMetadata path
  orig <- case eimg of
    Left _ -> do
      svg <- liftIO $ loadSvgFile path
      (img, _) <- liftIO $ renderSvgDocument emptyFontCache Nothing 100 $ fromJust svg
      return img
    Right (img, _) -> do
      return $ convertRGBA8 img
  let thumbName = FP.takeBaseName path ++ "_thumb.jpg"
      prevName = FP.takeBaseName path ++ "_preview.jpg"
      pathPrefix = "static" FP.</> "data" FP.</> T.unpack (extractKey uId) FP.</> T.unpack (extractKey aId)
      tPath = pathPrefix FP.</> thumbName
      pPath = pathPrefix FP.</> prevName
      -- origPix = convertRGBA8 orig
      oWidth = P.imageWidth orig :: Int
      oHeight = P.imageHeight orig :: Int
      tWidth = floor (fromIntegral oWidth / fromIntegral oHeight * fromIntegral tHeight :: Double)
      tHeight = 230 :: Int
      pHeight = 600 :: Int
      pScale = (fromIntegral pHeight :: Double) / (fromIntegral oHeight :: Double)
      pWidth = floor (fromIntegral oWidth * pScale)
      tPix = scale (tWidth, tHeight) orig
      pPix = scale (pWidth, pHeight) orig
  liftIO $ saveJpgImage 95 tPath $ ImageRGBA8 tPix
  liftIO $ saveJpgImage 95 pPath $ ImageRGBA8 pPix
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

writeOnDrive :: FileInfo -> UserId -> AlbumId -> Handler FP.FilePath
writeOnDrive fil userId albumId = do
  --filen <- return $ fileName fil
  album <- runDB $ getJust albumId
  let [PersistInt64 int] = if albumContent album == [] then [PersistInt64 1] else keyToValues $ maximum $ albumContent album
  let filen = show $ fromIntegral int + 1
  let ext = FP.takeExtension $ T.unpack $ fileName fil
  let path = "static" FP.</> "data" FP.</> T.unpack (extractKey userId) FP.</> T.unpack (extractKey albumId) FP.</> filen ++ ext
  dde <- liftIO $ doesDirectoryExist $ FP.dropFileName path
  if not dde
    then
      liftIO $ createDirectoryIfMissing True $ FP.dropFileName path
    else
      return ()
  liftIO $ fileMove fil path
  return path

dUploadForm :: UserId -> AlbumId -> AForm Handler FileBulk
dUploadForm userId albumId = FileBulk
  <$> areq textField (bfs ("Title" :: T.Text)) Nothing
  <*> areq multiFileField "Select file(s)" Nothing
  <*> lift (liftIO getCurrentTime)
  <*> pure userId
  <*> aopt textareaField (bfs ("Description" :: T.Text)) Nothing
  <*> areq tagField (bfs ("Enter tags" :: T.Text)) Nothing
  <*> pure albumId
  <*  bootstrapSubmit ("Upload" :: BootstrapSubmit T.Text)
      

data FileBulk = FileBulk
  { fileBulkPrefix :: T.Text
  , fileBulkFiles :: [FileInfo]
  , fileBulkTime :: UTCTime
  , fileBulkOwner :: UserId
  , fileBulkDesc :: Maybe Textarea
  , fileBulkTags :: [T.Text]
  , fileBulkAlbum :: AlbumId
  }

getUploadR :: Handler Html
getUploadR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      let userId = getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      let albums = userAlbums user
      if
        I.null albums
        then do
          setMessage "Please create an album first"
          redirect NewAlbumR
        else do
          (uploadWidget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ bulkUploadForm userId
          defaultLayout $ do
            setTitle "Eidolon :: Upload Medium"
            $(widgetFile "bulkUpload")
    Nothing -> do
      setMessage "You need to be logged in"
      redirect LoginR

bulkUploadForm :: UserId -> AForm Handler FileBulk
bulkUploadForm userId = (\a b c d e f g -> FileBulk b c d e f g a)
  <$> areq (selectField albums) (bfs ("Album" :: T.Text)) Nothing
  <*> areq textField (bfs ("Title" :: T.Text)) Nothing
  <*> areq multiFileField "Select file(s)" Nothing
  <*> lift (liftIO getCurrentTime)
  <*> pure userId
  <*> aopt textareaField (bfs ("Description" :: T.Text)) Nothing
  <*> areq tagField (bfs ("Enter tags" :: T.Text)) Nothing
  <*  bootstrapSubmit ("Upload" :: BootstrapSubmit T.Text)
  where
    albums = do
      allEnts <- runDB $ selectList [] [Desc AlbumTitle]
      let entities = catMaybes $ map (\ent ->
            if
              userId == albumOwner (entityVal ent) || userId `elem` albumShares (entityVal ent)
              then Just ent
              else Nothing
            ) allEnts
      optionsPairs $ I.map (\alb -> (albumTitle $ entityVal alb, entityKey alb)) entities

postUploadR :: Handler Html
postUploadR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      let userId = getUserIdFromText tempUserId
      ((result, _), _) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ bulkUploadForm userId
      case result of
        FormSuccess temp -> do
          let fils = fileBulkFiles temp
          let indFils = zip [1..] fils
          errNames <- mapM
            (\(index, file) -> do
              let mime = fileContentType file
              if mime `elem` acceptedTypes
                then do
                  let inAlbumId = fileBulkAlbum temp
                  albRef <- runDB $ getJust inAlbumId
                  let ownerId = albumOwner albRef
                  path <- writeOnDrive file ownerId inAlbumId
                  isOk <- liftIO $ checkCVE_2016_3714 path mime
                  if isOk
                    then do
                      meta <- generateThumbs path ownerId inAlbumId
                      tempName <- if
                        length indFils == 1
                        then return $ fileBulkPrefix temp
                        else
                          return
                            (fileBulkPrefix temp `T.append`
                            " " `T.append`
                            T.pack (show (index :: Int)) `T.append`
                            " of " `T.append`
                            T.pack (show (length indFils)))
                      let medium = Medium tempName ('/' : path) ('/' : metaThumbPath meta) mime (fileBulkTime temp) (fileBulkOwner temp) (fileBulkDesc temp) (fileBulkTags temp) inAlbumId ('/' : metaPreviewPath meta)
                      -- mId <- runDB $ I.insert medium
                      -- inALbum <- runDB $ getJust inAlbumId
                      -- let newMediaList = mId : albumContent inALbum
                      -- runDB $ update inAlbumId [AlbumContent =. newMediaList]
                      insertMedium medium inAlbumId
                      return Nothing
                    else do
                      liftIO $ removeFile (FP.normalise path)
                      return $ Just $ fileName file
                else
                  return $ Just $ fileName file
              ) indFils
          let onlyErrNames = removeItem Nothing errNames
          if
            L.null onlyErrNames
            then do
              setMessage "All images succesfully uploaded"
              redirect HomeR
            else do
              let justErrNames = map fromJust onlyErrNames
              let msg = Content $ Text.Blaze.Internal.Text $ "File type not supported of: " `T.append` T.intercalate ", " justErrNames
              setMessage msg
              redirect HomeR
        _ -> do
          setMessage "There was an error uploading the file"
          redirect UploadR
    Nothing -> do
      setMessage "You need to be logged in"
      redirect LoginR
