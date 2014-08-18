module Handler.Album where

import Import

getAlbumR :: UserId -> AlbumId -> Handler Html
getAlbumR ownerId albumId = do
  owner <- runDB $ getJust ownerId
  ownerName <- lift $ pure $ userName owner
  album <- runDB $ getJust albumId
  msu <- lookupSession "userId"
  presence <- case msu of
    Just tempUserId -> do
      userId <- lift $ pure $ getUserIdFromText tempUserId
      return (userId == ownerId)
    Nothing -> 
      return False
  media <- mapM (\a -> runDB $ getJust a) (albumContent album)
  defaultLayout $ do
    $(widgetFile "album")
