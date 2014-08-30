module Handler.Upload where

import Import as I
import Data.Time
import Data.Text
import System.FilePath

data TempMedium = TempMedium
  { tempMediumTitle :: Text
  , tempMediumFile :: FileInfo
  , tempMediumTime :: UTCTime
  , tempMediumOwner :: UserId
  , tempMediumDesc :: Textarea
  , tempMediumTags :: [Text]
  , tempMediumAlbum :: AlbumId
  }

getUploadR :: Handler Html
getUploadR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- lift $ pure $ getUserIdFromText tempUserId
      (uploadWidget, enctype) <- generateFormPost (uploadForm userId)
      defaultLayout $ do
        $(widgetFile "upload")
    Nothing -> do
      setMessage "You need to be logged in"
      redirect $ LoginR

postUploadR :: Handler Html
postUploadR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- lift $ pure $ getUserIdFromText tempUserId
      ((result, uploadWidget), enctype) <- runFormPost (uploadForm userId)
      case result of
        FormSuccess temp -> do
          path <- writeOnDrive (tempMediumFile temp) userId (tempMediumAlbum temp)
          inAlbumId <- return $ tempMediumAlbum temp
          medium <- return $ Medium
            (tempMediumTitle temp)
            ('/' : path)
            (tempMediumTime temp)
            (tempMediumOwner temp)
            (tempMediumDesc temp)
            (tempMediumTags temp)
            inAlbumId
          mId <- runDB $ I.insert medium
          inAlbum <- runDB $ getJust inAlbumId
          newMediaList <- return $ mId : (albumContent inAlbum)
          runDB $ update inAlbumId [AlbumContent =. newMediaList]
          setMessage "Image succesfully uploaded"
          redirect $ HomeR
        _ -> do
          setMessage "There was an error uploading the file"
          redirect $ UploadR
    Nothing -> do
      setMessage "You need to be logged in"
      redirect $ LoginR

getDirectUploadR :: AlbumId -> Handler Html
getDirectUploadR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of -- does the requested album exist
    Just album -> do
      ownerId <- return $ albumOwner album
      owner <- runDB $ getJust ownerId
      ownerName <- return $ userName owner
      msu <- lookupSession "userId"
      case msu of -- is anybody logged in
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          presence <- return (userId == ownerId)
          case presence of -- is the owner present
            True -> do
              (dUploadWidget, enctype) <- generateFormPost $ dUploadForm userId albumId
              defaultLayout $ do
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
      owner <- runDB $ getJust ownerId
      ownerName <- return $ userName owner
      msu <- lookupSession "userId"
      case msu of -- is anybody logged in
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          presence <- return (userId == ownerId)
          case presence of -- is the logged in user the owner
            True -> do
              ((result, dUploadWidget), enctype) <- runFormPost (dUploadForm userId albumId)
              case result of
                FormSuccess temp -> do
                  path <- writeOnDrive (tempMediumFile temp) userId albumId
                  medium <- return $ Medium
                    (tempMediumTitle temp)
                    ('/' : path)
                    (tempMediumTime temp)
                    (tempMediumOwner temp)
                    (tempMediumDesc temp)
                    (tempMediumTags temp)
                    albumId
                  mId <- runDB $ insert medium
                  inAlbum <- runDB $ getJust albumId
                  newMediaList <- return $ mId : (albumContent inAlbum)
                  runDB $ update albumId [AlbumContent =. newMediaList]
                  setMessage "Image successfully uploaded"
                  redirect $ AlbumR albumId
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

writeOnDrive :: FileInfo -> UserId -> AlbumId -> Handler FilePath
writeOnDrive file userId albumId = do
  filename <- return $ fileName file
  path <- return $ "static" </> "data"
    </> (unpack $ extractKey userId)
    </> (unpack $ extractKey albumId)
    </> (unpack filename)
  liftIO $ fileMove file path
  return path

uploadForm :: UserId -> Form TempMedium
uploadForm userId = renderDivs $ TempMedium
  <$> areq textField "Title" Nothing
  <*> areq fileField "Select file" Nothing
  <*> lift (liftIO getCurrentTime)
  <*> pure userId
  <*> areq textareaField "Description" Nothing
  <*> areq tagField "Enter tags" Nothing
  <*> areq (selectField albums) "Album" Nothing
  where
--    albums :: GHandler App App (OptionList AlbumId)
    albums = do
      entities <- runDB $ selectList [AlbumOwner ==. userId] [Desc AlbumTitle]
      optionsPairs $ I.map (\alb -> (albumTitle $ entityVal alb, entityKey alb)) entities

dUploadForm :: UserId -> AlbumId -> Form TempMedium
dUploadForm userId albumId = renderDivs $ TempMedium
  <$> areq textField "Title" Nothing
  <*> areq fileField "Select file" Nothing
  <*> lift (liftIO getCurrentTime)
  <*> pure userId
  <*> areq textareaField "Description" Nothing
  <*> areq tagField "Enter tags" Nothing
  <*> pure albumId
