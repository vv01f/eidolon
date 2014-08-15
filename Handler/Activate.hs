module Handler.Activate where

import Import as I
import Data.Text
import System.Directory
import System.FilePath

getActivateR :: Text -> Handler Html
getActivateR token = do
  t <- runDB $ selectFirst [TokenToken ==. token] []
  case t of
    Nothing -> do
      setMessage "Invalid Token!"
      redirect $ HomeR
    Just x -> do
      userId <- runDB $ insert $ tokenUser (entityVal x)
      liftIO $ createDirectoryIfMissing True $ "data" </> (unpack $ extractKey userId)
      setMessage $ "User activated"
      redirect $ HomeR
