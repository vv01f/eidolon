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
import Data.Time
import Data.Maybe
import qualified Data.Text as T
import Data.List as L
import qualified System.FilePath as FP
import Filesystem.Path.CurrentOS
import Graphics.ImageMagick.MagickWand
import Text.Blaze.Internal

getDirectUploadR :: AlbumId -> Handler Html
getDirectUploadR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of -- does the requested album exist
    Just album -> do
      ownerId <- return $ albumOwner album
      msu <- lookupSession "userId"
      case msu of -- is anybody logged in
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          presence <- return $ (userId == ownerId) || (userId `elem` (albumShares album))
          case presence of -- is the owner present or a user with whom the album is shared
            True -> do
              (dUploadWidget, enctype) <- generateFormPost $ dUploadForm userId albumId
              formLayout $ do
                setTitle $ toHtml ("Eidolon :: Upload medium to " `T.append` (albumTitle album))
                $(widgetFile "dUpload")
            False -> do
              setMessage "You must own this album to upload"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to upload"
          redirect $ LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect $ HomeR

postDirectUploadR :: AlbumId -> Handler Html
postDirectUploadR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of -- does the album exist
    Just album -> do
      ownerId <- return $ albumOwner album
      msu <- lookupSession "userId"
      case msu of -- is anybody logged in
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          presence <- return $ (userId == ownerId) || (userId `elem` (albumShares album))
          case presence of -- is the logged in user the owner or is the album shared with him
            True -> do
              ((result, _), _) <- runFormPost (dUploadForm userId albumId)
              case result of
                FormSuccess temp -> do
                  fils <- return $ fileBulkFiles temp
                  indFils <- return $ zip [1..] fils
                  errNames <- mapM
                    (\(index, file) -> do
                      mime <- return $ fileContentType file
                      case mime `elem` acceptedTypes of
                        True -> do
                          path <- writeOnDrive file ownerId albumId
                          (thumbPath, iWidth, tWidth) <- generateThumb path ownerId albumId
                          tempName <- case length indFils == 1 of
                            False -> return $ ((fileBulkPrefix temp) `T.append` " " `T.append` (T.pack (show index)) `T.append` " of " `T.append` (T.pack (show (length indFils))))
                            True -> return $ fileBulkPrefix temp
                          medium <- return $ Medium
                            tempName
                            ('/' : path)
                            ('/' : thumbPath)
                            mime
                            (fileBulkTime temp)
                            (fileBulkOwner temp)
                            (fileBulkDesc temp)
                            (fileBulkTags temp)
                            iWidth
                            tWidth
                            albumId
                          mId <- runDB $ I.insert medium
                          inALbum <- runDB $ getJust albumId
                          newMediaList <- return $ mId : (albumContent inALbum)
                          runDB $ update albumId [AlbumContent =. newMediaList]
                          return Nothing
                        False -> do
                          return $ Just $ fileName file
                      ) indFils
                  onlyErrNames <- return $ removeItem Nothing errNames
                  case L.null onlyErrNames of
                    True -> do
                      setMessage "All images succesfully uploaded"
                      redirect $ HomeR
                    False -> do
                      justErrNames <- return $ map fromJust onlyErrNames
                      msg <- return $ Content $ Text $ "File type not supported of: " `T.append` (T.intercalate ", " justErrNames)
                      setMessage msg
                      redirect $ HomeR
                _ -> do
                  setMessage "There was an error uploading the file"
                  redirect $ DirectUploadR albumId
            False -> do -- owner is not present
              setMessage "You must own this album to upload"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to upload"
          redirect $ AlbumR albumId
    Nothing -> do
      setMessage "This Album does not exist"
      redirect $ AlbumR albumId

generateThumb :: FP.FilePath -> UserId -> AlbumId -> Handler (FP.FilePath, Int, Int)
generateThumb path userId albumId = do
  newName <- return $ (FP.takeBaseName path) ++ "_thumb.jpg"
  newPath <- return $ "static" FP.</> "data"
    FP.</> (T.unpack $ extractKey userId)
    FP.</> (T.unpack $ extractKey albumId)
    FP.</> newName
  (iWidth, tWidth) <- liftIO $ withMagickWandGenesis $ do
    (_ , w) <- magickWand
    p <- pixelWand
    readImage w (decodeString path)
    w1 <- getImageWidth w
    h1 <- getImageHeight w
    h2 <- return 230
    w2 <- return $ floor (((fromIntegral w1) / (fromIntegral h1)) * (fromIntegral h2) :: Double)
    setImageAlphaChannel w deactivateAlphaChannel
    setImageFormat w "jpeg"
    resizeImage w w2 h2 lanczosFilter 1
    setImageCompressionQuality w 95
    writeImage w (Just (decodeString newPath))
    return (w1, w2)
  return (newPath, iWidth, tWidth)

writeOnDrive :: FileInfo -> UserId -> AlbumId -> Handler FP.FilePath
writeOnDrive fil userId albumId = do
  --filen <- return $ fileName fil
  album <- runDB $ getJust albumId
  filen <- return $ show $ (length $ albumContent album) + 1
  ext <- return $ FP.takeExtension $ T.unpack $ fileName fil
  path <- return $ "static" FP.</> "data"
    FP.</> (T.unpack $ extractKey userId)
    FP.</> (T.unpack $ extractKey albumId)
    FP.</> filen ++ ext
  liftIO $ fileMove fil path
  return path

dUploadForm :: UserId -> AlbumId -> Form FileBulk
dUploadForm userId albumId = renderDivs $ FileBulk
  <$> areq textField "Title" Nothing
  <*> areq multiFileField "Select file(s)" Nothing
  <*> lift (liftIO getCurrentTime)
  <*> pure userId
  <*> areq textareaField "Description" Nothing
  <*> areq tagField "Enter tags" Nothing
  <*> pure albumId

data FileBulk = FileBulk
  { fileBulkPrefix :: Text
  , fileBulkFiles :: [FileInfo]
  , fileBulkTime :: UTCTime
  , fileBulkOwner :: UserId
  , fileBulkDesc :: Textarea
  , fileBulkTags :: [Text]
  , fileBulkAlbum :: AlbumId
  }

getUploadR :: Handler Html
getUploadR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      albums <- return $ userAlbums user
      case I.null albums of
        False -> do
          (uploadWidget, enctype) <- generateFormPost (bulkUploadForm userId)
          formLayout $ do
            setTitle "Eidolon :: Upload Medium"
            $(widgetFile "bulkUpload")
        True -> do
          setMessage "Please create an album first"
          redirect $ NewAlbumR
    Nothing -> do
      setMessage "You need to be logged in"
      redirect $ LoginR

bulkUploadForm :: UserId -> Form FileBulk
bulkUploadForm userId = renderDivs $ (\a b c d e f g -> FileBulk b c d e f g a)
  <$> areq (selectField albums) "Album" Nothing
  <*> areq textField "Title" Nothing
  <*> areq multiFileField "Select file(s)" Nothing
  <*> lift (liftIO getCurrentTime)
  <*> pure userId
  <*> areq textareaField "Description" Nothing
  <*> areq tagField "Enter tags" Nothing
  where
    albums = do
      allEnts <- runDB $ selectList [] [Desc AlbumTitle]
      entities <- return $
        map fromJust $
        removeItem Nothing $ map
          (\ent -> do
            case (userId == (albumOwner $ entityVal ent)) || (userId `elem` (albumShares $ entityVal ent)) of
              True -> Just ent
              False -> Nothing
            ) allEnts
      optionsPairs $ I.map (\alb -> (albumTitle $ entityVal alb, entityKey alb)) entities

postUploadR :: Handler Html
postUploadR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- lift $ pure $ getUserIdFromText tempUserId
      ((result, _), _) <- runFormPost (bulkUploadForm userId)
      case result of
        FormSuccess temp -> do
          fils <- return $ fileBulkFiles temp
          indFils <- return $ zip [1..] fils
          errNames <- mapM
            (\(index, file) -> do
              mime <- return $ fileContentType file
              case mime `elem` acceptedTypes of
                True -> do
                  inAlbumId <- return $ fileBulkAlbum temp
                  albRef <- runDB $ getJust inAlbumId
                  ownerId <- return $ albumOwner albRef
                  path <- writeOnDrive file ownerId inAlbumId
                  (thumbPath, iWidth, tWidth) <- generateThumb path ownerId inAlbumId
                  tempName <- case length indFils == 1 of
                    False -> return $ ((fileBulkPrefix temp) `T.append` " " `T.append` (T.pack (show index)) `T.append` " of " `T.append` (T.pack (show (length indFils))))
                    True -> return $ fileBulkPrefix temp
                  medium <- return $ Medium
                    tempName
                    ('/' : path)
                    ('/' : thumbPath)
                    mime
                    (fileBulkTime temp)
                    (fileBulkOwner temp)
                    (fileBulkDesc temp)
                    (fileBulkTags temp)
                    iWidth
                    tWidth
                    inAlbumId
                  mId <- runDB $ I.insert medium
                  inALbum <- runDB $ getJust inAlbumId
                  newMediaList <- return $ mId : (albumContent inALbum)
                  runDB $ update inAlbumId [AlbumContent =. newMediaList]
                  return Nothing
                False -> do
                  return $ Just $ fileName file
              ) indFils
          onlyErrNames <- return $ removeItem Nothing errNames
          case L.null onlyErrNames of
            True -> do
              setMessage "All images succesfully uploaded"
              redirect $ HomeR
            False -> do
              justErrNames <- return $ map fromJust onlyErrNames
              msg <- return $ Content $ Text $ "File type not supported of: " `T.append` (T.intercalate ", " justErrNames)
              setMessage msg
              redirect $ HomeR
        _ -> do
          setMessage "There was an error uploading the file"
          redirect $ UploadR
    Nothing -> do
      setMessage "You need to be logged in"
      redirect $ LoginR
