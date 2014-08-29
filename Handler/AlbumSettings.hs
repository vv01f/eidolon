module Handler.AlbumSettings where

import Import
import qualified Data.Text as T
import System.Directory
import System.FilePath

getAlbumSettingsR :: AlbumId -> Handler Html
getAlbumSettingsR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of
    Just album -> do
      ownerId <- return $ albumOwner album
      owner <- runDB $ getJust ownerId
      ownerName <- return $ userName owner
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          presence <- return (userId == ownerId)
          case presence of
            True -> do
              (albumSettingsWidget, enctype) <- generateFormPost $ albumSettingsForm album albumId
              defaultLayout $ do
                $(widgetFile "albumSettings")
            False -> do
              setMessage "You must own this album to change its settings"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to change settings"
          redirect $ LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect $ HomeR

postAlbumSettingsR :: AlbumId -> Handler Html
postAlbumSettingsR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of
    Just album -> do
      ownerId <- return $ albumOwner album
      owner <- runDB $ getJust ownerId
      ownerName <- return $ userName owner
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          presence <- return (userId == ownerId)
          case presence of
            True -> do
              ((result, albumSettingsWidget), enctype) <- runFormPost $ albumSettingsForm album albumId
              case result of
                FormSuccess temp -> do
                  aId <- runDB $ update albumId 
                    [ AlbumTitle =. albumTitle temp
                    , AlbumOwner =. albumOwner temp
                    , AlbumContent =. albumContent temp
                    , AlbumSamplePic =. albumSamplePic temp
                    ]
                  setMessage "Album settings changed succesfully"
                  redirect $ AlbumR albumId
                _ -> do
                  setMessage "There was an error changing the settings"
                  redirect $ AlbumSettingsR albumId
            False -> do
              setMessage "You must own this album to change its settings"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to change settings"
          redirect $ LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect $ HomeR

albumSettingsForm :: Album -> AlbumId -> Form Album
albumSettingsForm album albumId = renderDivs $ Album
  <$> areq textField "Title" (Just $ albumTitle album)
  <*> pure (albumOwner album)
  <*> pure (albumContent album)
  <*> aopt (selectField media) "Sample picture" (Just $ albumSamplePic album)
  where
    media = do
      entities <- runDB $ selectList [MediumAlbum ==. albumId] [Desc MediumTitle]
      optionsPairs $ map (\med -> (mediumTitle $ entityVal med, mediumPath (entityVal med))) entities

getAlbumDeleteR :: AlbumId -> Handler Html
getAlbumDeleteR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of
    Just album -> do
      ownerId <- return $ albumOwner album
      owner <- runDB $ getJust ownerId
      ownerName <- return $ userName owner
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          presence <- return (userId == ownerId)
          case presence of
            True -> do
              defaultLayout $ do
                $(widgetFile "albumDelete")
            False -> do
              setMessage "You must own this album to delete it"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to delete albums"
          redirect $ LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect $ HomeR

postAlbumDeleteR :: AlbumId -> Handler Html
postAlbumDeleteR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of
    Just album -> do
      ownerId <- return $ albumOwner album
      owner <- runDB $ getJust ownerId
      ownerName <- return $ userName owner
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          presence <- return (userId == ownerId)
          case presence of
            True -> do
              confirm <- lookupPostParam "confirm"
              case confirm of
                Just "confirm" -> do
                  mapM (\a -> runDB $ delete a) (albumContent album)
                  runDB $ delete albumId
                  liftIO $ removeDirectoryRecursive $ "static" </> "data" </> (T.unpack $ extractKey userId) </> (T.unpack $ extractKey albumId)
                  setMessage "Album deleted succesfully"
                  redirect $ HomeR
            False -> do
              setMessage "You must own this album to delete it"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to delete albums"
          redirect $ LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect $ HomeR
