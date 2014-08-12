module Handler.Activate where

import Import as I

getActivateR :: Text -> Handler Html
getActivateR token = do
  t <- runDB $ selectList [TokenToken ==. token] []
  case t of
    [] -> do
      setMessage $ [shamlet|<pre>Invalid Token!|]
      redirect $ HomeR
    [x] -> do
      _ <- runDB $ insert $ tokenUser (entityVal x)
      setMessage $ [shamlet|<pre>User activated|]
      redirect $ HomeR
    _ -> do
      error "unexpected error"
