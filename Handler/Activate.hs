module Handler.Activate where

import Import as I
import Data.Text as T

getActivateR :: String -> Handler Html
getActivateR token = do
  t <- runDB $ selectList [TokenToken ==. (T.pack token)] []
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
