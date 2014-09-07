module Handler.AdminAlbumSettings where

import Import
import qualified Data.Text as T
import System.FilePath
import System.Directory

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
getAdminAlbumSettingsR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of
    Just album -> do
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          user <- runDB $ getJust userId
          case userAdmin user of
            True -> do
              (adminAlbumSettingsWidget, enctype) <- generateFormPost $ adminAlbumSettingsForm album albumId
              defaultLayout $ do
                $(widgetFile "adminAlbumSet")
            False -> do
              setMessage "You must be admin"
              redirect $ HomeR
        Nothing -> do
          setMessage "You must be logged in"
          redirect $ LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect $ AdminR

postAdminAlbumSettingsR :: AlbumId -> Handler Html
postAdminAlbumSettingsR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of
    Just album -> do
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          user <- runDB $ getJust userId
          case userAdmin user of
            True -> do
              ((res, adminAlbumSettingsWidget), enctype) <- runFormPost $ adminAlbumSettingsForm album albumId
              case res of
                FormSuccess temp -> do
                  aId <- runDB $ update albumId
                    [ AlbumTitle =. albumTitle temp
                    , AlbumSamplePic =. albumSamplePic temp
                    ]
                  setMessage "Album settings changed successfully"
                  redirect $ AdminR
                _ -> do
                  setMessage "There was an error while changing the settings"
                  redirect $ AdminAlbumSettingsR albumId
            False -> do
              setMessage "You must be admin"
              redirect $ HomeR
        Nothing -> do
          setMessage "You must be logged in"
          redirect $ LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect $ AdminR

adminAlbumSettingsForm :: Album -> AlbumId -> Form Album
adminAlbumSettingsForm album albumId = renderDivs $ Album
  <$> areq textField "Title" (Just $ albumTitle album)
  <*> pure (albumOwner album)
  <*> pure (albumContent album)
  <*> aopt (selectField media) "Sample picture" (Just $ albumSamplePic album)
  where
    media = do
      entities <- runDB $ selectList [MediumAlbum ==. albumId] [Desc MediumTitle]
      optionsPairs $ map (\med -> (mediumTitle $ entityVal med, mediumPath (entityVal med))) entities

getAdminAlbumDeleteR :: AlbumId -> Handler Html
getAdminAlbumDeleteR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of
    Just album -> do
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          user <- runDB $ getJust userId
          case userAdmin user of
            True -> do
              -- remove reference from owner
              ownerId <- return $ albumOwner album
              owner <- runDB $ getJust ownerId
              albumList <- return $ userAlbums owner
              newAlbumList <- return $ removeItem albumId albumList
              -- delete album content
              mapM (\a -> runDB $ delete a) (albumContent album)
              -- delete album
              runDB $ delete albumId
              -- delete files
              liftIO $ removeDirectoryRecursive $ "static" </> "data" </> (T.unpack $ extractKey ownerId) </> (T.unpack $ extractKey albumId)
              -- outro
              setMessage "Album deleted successfully"
              redirect $ AdminR
            _ -> do
              setMessage "You are no admin"
              redirect $ HomeR
        Nothing -> do
          setMessage "You must be logged in"
          redirect $ LoginR
    Nothing -> do
      setMessage "This Album does not exist"
      redirect $ AdminR
