module Handler.Upload where

import Import as I
import Data.Time
import Data.Text
import System.FilePath
import Database.Persist.Types

data TempMedium = TempMedium
  { tempMediumTitle :: Text
  , tempMediumFile :: FileInfo
  , tempMediumTime :: UTCTime
  , tempMediumOwner :: UserId
  , tempMediumDesc :: Textarea
  , tempMediumTags :: [Text]
  }

getUploadR :: Handler Html
getUploadR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- lift $ getUserIdFromText tempUserId
      (uploadWidget, enctype) <- generateFormPost (uploadForm userId)
      defaultLayout $ do
        $(widgetFile "upload")
    Nothing -> do
      setMessage $ [shamlet|<pre>You need to be logged in|]
      redirect $ LoginR

--getUserIdFromText :: Text -> UserId
getUserIdFromText tempUserId =
  Key $ PersistInt64 $ fromIntegral $ read $ unpack tempUserId

postUploadR :: Handler Html
postUploadR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- lift $ getUserIdFromText tempUserId
      ((result, uploadWidget), enctype) <- runFormPost (uploadForm userId)
      case result of
        FormSuccess temp -> do
          path <- writeOnDrive $ tempMediumFile temp
          medium <- return $ Medium
            (tempMediumTitle temp)
            path
            (tempMediumTime temp)
            (tempMediumOwner temp)
            (tempMediumDesc temp)
            (tempMediumTags temp)
          mId <- runDB $ insert medium
          setMessage $ [shamlet|Image succesfully uploaded|]
          redirect $ HomeR
        _ -> do
          setMessage $ [shamlet|There was an error uploading the file|]
          redirect $ UploadR
    Nothing -> do
      setMessage $ [shamlet|<pre>You need to be logged in|]
      redirect $ LoginR

writeOnDrive :: FileInfo -> Handler FilePath
writeOnDrive file = do
  filename <- return $ fileName file
  path <- return $ "static" </> (unpack filename)
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
--  <*> areq (selectField albums) "Album" Nothing
--  where
--    albums :: Handler App App (OptionList AlbumId)
--    albums = do
--      runDB $ selectList [AlbumOwner ==. userId] [Desc AlbumTitle]

tagField :: Field Handler [Text]
tagField = Field
  { fieldParse = \rawVals _ -> do
      case rawVals of
        [x] -> return $ Right $ Just $ splitOn " " x
  , fieldView = \idAttr nameAttr _ eResult isReq ->
      [whamlet|<input id=#{idAttr} type="text" name=#{nameAttr}>|]
  , fieldEnctype = UrlEncoded
  }
