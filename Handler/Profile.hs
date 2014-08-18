module Handler.Profile where

import Import
import Data.Maybe

getProfileR :: UserId -> Handler Html
getProfileR ownerId = do
  owner <- runDB $ get ownerId
  ownerName <- lift $ pure $ userName $ fromJust owner
  userAlbs <- runDB $ selectList [AlbumOwner ==. ownerId] [Desc AlbumTitle]
  recentMedia <- (runDB $ selectList [MediumOwner ==. ownerId] [Desc MediumTime])
  msu <- lookupSession "userId"
  presence <- case msu of
    Just tempUserId -> do
      userId <- lift $ pure $ getUserIdFromText tempUserId
      return (userId == ownerId)
    Nothing ->
      return False
  defaultLayout $ do
    $(widgetFile "profile")
