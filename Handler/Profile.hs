module Handler.Profile where

import Import

getProfileR :: Text -> Handler Html
getProfileR username = do
  msu <- lookupSession "username"
  userMedia <- runDB $ selectList [MediumOwner ==. username] [Desc MediumTime]
  defaultLayout $ do
    $(widgetFile "profile")
