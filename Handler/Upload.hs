module Handler.Upload where

import Import as I
import Data.Time
import Data.Text
import System.FilePath

data TempMedium = TempMedium
  { tempMediumTitle :: Text
  , tempMediumFile :: FileInfo
  , tempMediumTime :: UTCTime
  , tempMediumOwner :: Text
  , tempMediumDesc :: Textarea
  , tempMediumTags :: [Text]
  }

getUploadR :: Handler Html
getUploadR = do
  msu <- lookupSession "username"
  case msu of
    Just username -> do
      (uploadWidget, enctype) <- generateFormPost (uploadForm username)
      defaultLayout $ do
        $(widgetFile "upload")
    Nothing -> do
      setMessage $ [shamlet|<pre>You need to be logged in|]
      redirect $ LoginR

postUploadR :: Handler Html
postUploadR = do
  msu <- lookupSession "username"
  case msu of
    Just username -> do
      ((result, uploadWidget), enctype) <- runFormPost (uploadForm username)
      case result of
        FormSuccess temp -> do
          path <- writeOnDrive username $ tempMediumFile temp
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

writeOnDrive :: Text -> FileInfo -> Handler FilePath
writeOnDrive username file = do
  filename <- return $ fileName file
  path <- return $ "static" </> (unpack filename)
  liftIO $ fileMove file path
  return path

uploadForm :: Text -> Form TempMedium
uploadForm username = renderDivs $ TempMedium
  <$> areq textField "Title" Nothing
  <*> areq fileField "Select file" Nothing
  <*> lift (liftIO getCurrentTime)
  <*> pure username
  <*> areq textareaField "Description" Nothing
  <*> areq tagField "Enter tags" Nothing

tagField :: Field Handler [Text]
tagField = Field
  { fieldParse = \rawVals _ -> do
      case rawVals of
        [x] -> return $ Right $ Just $ splitOn " " x
  , fieldView = \idAttr nameAttr _ eResult isReq ->
      [whamlet|<input id=#{idAttr} type="text" name=#{nameAttr}>|]
  , fieldEnctype = UrlEncoded
  }
