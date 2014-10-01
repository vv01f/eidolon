module Handler.Upload where

import Import as I
import Data.Time
import qualified Data.Text as T
import System.FilePath
import Graphics.Transform.Magick.Images
import Graphics.Transform.Magick.Types
import Foreign
import Foreign.C.Types
import Foreign.C.String

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
        setTitle "Eidolon :: Upload Medium"
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
          thumbPath <- generateThumb path userId (tempMediumAlbum temp)
          inAlbumId <- return $ tempMediumAlbum temp
          medium <- return $ Medium
            (tempMediumTitle temp)
            ('/' : path)
            ('/' : thumbPath)
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
                  thumbPath <- generateThumb path ownerId albumId
                  medium <- return $ Medium
                    (tempMediumTitle temp)
                    ('/' : path)
                    ('/' : thumbPath)
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

generateThumb :: FilePath -> UserId -> AlbumId -> Handler FilePath
generateThumb path userId albumId = do
  liftIO $ initializeMagick
  image <- liftIO $ readImage path
  case image of
    HImage imag _ -> do
      h1 <- liftIO $ withForeignPtr imag (\a -> do
        himage <- peek a
        r <- return $ rows himage
        case r of CULong ro -> return ro
        )
      w1 <- liftIO $ withForeignPtr imag (\a -> do
        himage <- peek a
        c <- return $ columns himage
        case c of CULong co -> return co
        )
      h2 <- return $ 220
      w2 <- return $ (h1 `div` w1) * h2
      error $ show w2
      thumb <- return $ thumbnailImage (fromIntegral w2) (fromIntegral h2) image
      newName <- return $ (takeBaseName path) ++ "_thumb" ++ (takeExtension path)
      newPath <- return $ "static" </> "data" 
        </> (T.unpack $ extractKey userId)
        </> (T.unpack $ extractKey albumId)
        </> newName
      _ <- liftIO $ writeImage newPath thumb
      return newPath

writeOnDrive :: FileInfo -> UserId -> AlbumId -> Handler FilePath
writeOnDrive file userId albumId = do
  filen <- return $ fileName file
  path <- return $ "static" </> "data"
    </> (T.unpack $ extractKey userId)
    </> (T.unpack $ extractKey albumId)
    </> (T.unpack filen)
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
