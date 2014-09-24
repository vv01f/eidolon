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
          albums <- runDB $ selectList [] [Asc AlbumTitle]
          defaultLayout $ do
            setTitle "Administration: Albums"
            $(widgetFile "adminAlbums")
        False -> do
          setMessage "You are no admin"
          redirect $ HomeR
    Nothing -> do
      setMessage "You must be logged in"
      redirect $ LoginR

getAdminAlbumMediaR :: AlbumId -> Handler Html
getAdminAlbumMediaR albumId = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True -> do
          tempAlbum <- runDB $ get albumId
          case tempAlbum of
            Just album -> do
              media <- runDB $ selectList [MediumAlbum ==. albumId] [Asc MediumTitle]
              defaultLayout $ do
                setTitle "Administration: Album media"
                $(widgetFile "adminAlbumMedia")
            Nothing -> do
              setMessage "This album does not exist"
              redirect $ AdminR
        False -> do
          setMessage "You are no admin"
          redirect $ HomeR
    Nothing -> do
      setMessage "You must be logged in"
      redirect $ LoginR

getAdminAlbumSettingsR :: AlbumId -> Handler Html
getAdminAlbumSettingsR albumId = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True -> do
          tempAlbum <- runDB $ get albumId
          case tempAlbum of
            Just album -> do
              (adminAlbumSettingsWidget, enctype) <- generateFormPost $ adminAlbumSettingsForm album albumId
              defaultLayout $ do
                setTitle "Administration: Album settings"
                $(widgetFile "adminAlbumSet")
            Nothing -> do
              setMessage "This album does not exist"
              redirect $ AdminR
        False -> do
          setMessage "You are no admin"
          redirect $ HomeR
    Nothing -> do
      setMessage "You must be logged in"
      redirect $ LoginR

postAdminAlbumSettingsR :: AlbumId -> Handler Html
postAdminAlbumSettingsR albumId = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True -> do
          tempAlbum <- runDB $ get albumId
          case tempAlbum of
            Just album -> do
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
            Nothing -> do
              setMessage "This album does not exist"
              redirect $ AdminR
        False -> do
          setMessage "You are no admin"
          redirect $ HomeR
    Nothing -> do
      setMessage "You must be logged in"
      redirect $ LoginR

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
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True -> do
          tempAlbum <- runDB $ get albumId
          case tempAlbum of
            Just album -> do
              -- remove reference from owner
              ownerId <- return $ albumOwner album
              owner <- runDB $ getJust ownerId
              albumList <- return $ userAlbums owner
              newAlbumList <- return $ removeItem albumId albumList
              runDB $ update ownerId [UserAlbums =. newAlbumList]
              -- delete album content
              mapM (\a -> runDB $ delete a) (albumContent album)
              -- delete album
              runDB $ delete albumId
              -- delete files
              liftIO $ removeDirectoryRecursive $ "static" </> "data" </> (T.unpack $ extractKey ownerId) </> (T.unpack $ extractKey albumId)
              -- outro
              setMessage "Album deleted successfully"
              redirect $ AdminR
            Nothing -> do
              setMessage "This album dies not exist"
              redirect $ AdminR
        False -> do
          setMessage "You are no admin"
          redirect $ HomeR
    Nothing -> do
      setMessage "You must be logged in"
      redirect $ LoginR
