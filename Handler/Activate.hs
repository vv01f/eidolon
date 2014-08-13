module Handler.Activate where

import Import as I

getActivateR :: Text -> Handler Html
getActivateR token = do
  t <- runDB $ selectFirst [TokenToken ==. token] []
  case t of
    Nothing -> do
      setMessage $ [shamlet|<pre>Invalid Token!|]
      redirect $ HomeR
    Just x -> do
      _ <- runDB $ insert $ tokenUser (entityVal x)
      setMessage $ [shamlet|<pre>User activated|]
      redirect $ HomeR
