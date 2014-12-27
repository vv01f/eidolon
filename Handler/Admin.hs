module Handler.Admin where

import Import
import Helper

getAdminR :: Handler Html
getAdminR = do
  adminCheck <- loginIsAdmin
  case adminCheck of
    Right _ -> do
      defaultLayout $ do
        setTitle "Administration: Menu"
        $(widgetFile "adminBase")
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect $ route
