module Handler.Profile where

import Import
import Data.Maybe
import qualified Data.Text as T

getProfileR :: UserId -> Handler Html
getProfileR ownerId = do
  tempOwner <- runDB $ get ownerId
  case tempOwner of
    Just owner -> do
      ownerSlug <- lift $ pure $ userSlug owner
      userAlbs <- runDB $ selectList [AlbumOwner ==. ownerId] [Asc AlbumTitle]
      recentMedia <- (runDB $ selectList [MediumOwner ==. ownerId] [Desc MediumTime])
      msu <- lookupSession "userId"
      presence <- case msu of
        Just tempUserId -> do
          userId <- lift $ pure $ getUserIdFromText tempUserId
          return (userId == ownerId)
        Nothing ->
          return False
      defaultLayout $ do
        setTitle $ toHtml ("Eidolon :: " `T.append` (userSlug owner) `T.append` "'s profile")
        $(widgetFile "profile")
    Nothing -> do
      setMessage "This profile does not exist"
      redirect $ HomeR

getUserR :: Text -> Handler Html
getUserR ownerName = do
  tempOwner <- runDB $ selectFirst [UserName ==. ownerName] []
  case tempOwner of
    Just (Entity ownerId owner) ->
      getProfileR ownerId
    Nothing -> do
      setMessage "This user does not exist"
      redirect $ HomeR
