module Handler.Album where

import Import

getAlbumR :: AlbumId -> Handler Html
getAlbumR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of
    Just album -> do
      ownerId <- return $ albumOwner album
      owner <- runDB $ getJust ownerId
      ownerName <- return $ userName owner
      msu <- lookupSession "userId"
      presence <- case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          return (userId == ownerId)
        Nothing ->
         return False
--      media <- mapM (\a -> runDB $ getJust a) (albumContent album)
      media <- runDB $ selectList [MediumAlbum ==. albumId] [Desc MediumTime]
      defaultLayout $ do
        $(widgetFile "album")
    Nothing -> do
      setMessage "This album does not exist"
      redirect $ HomeR
