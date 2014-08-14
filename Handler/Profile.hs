module Handler.Profile where

import Import

getProfileR :: UserId -> Handler Html
getProfileR userId = error "reworking logic" -- do
--  msu <- lookupSession "id"
--  case msu of
--    Just tempUserId -> do
--      userId <- Key $ PersistInt64 $ fromIntegral tempUserId
--      userMedia <- runDB $ selectList [MediumOwner ==. userId] [Desc MediumTime]
--      defaultLayout $ do
--        $(widgetFile "profile")
