module Handler.Admin where

import Import

getAdminR :: Handler Html
getAdminR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True -> do
          defaultLayout $ do
            $(widgetFile "adminBase")
        False -> do
          setMessage "You have no admin rights"
          redirect $ HomeR
    Nothing -> do
      setMessage "You are not logged in"
      redirect $ LoginR
