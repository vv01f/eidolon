module Handler.AdminAlbumSettings where

import Import

getAdminAlbumsR :: Handler Html
getAdminAlbumsR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True -> do
          albums <- runDB $ selectList [] [Desc AlbumTitle]
          defaultLayout $ do
            $(widgetFile "adminAlbums")
        False -> do
          setMessage "You are no admin"
          redirect $ HomeR
    Nothing -> do
      setMessage "You must be logged in"
      redirect $ LoginR

getAdminAlbumSettingsR :: AlbumId -> Handler Html
getAdminAlbumSettingsR = error "Not yet implemented: getAdminAlbumSettingsR"

postAdminAlbumSettingsR :: AlbumId -> Handler Html
postAdminAlbumSettingsR = error "Not yet implemented: postAdminAlbumSettingsR"

getAdminAlbumDeleteR :: AlbumId -> Handler Html
getAdminAlbumDeleteR albumId = error "Not yet implemented: getAdminAlbumDeleteR" 
