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
          ownerPresence <- return (userId == ownerId)
          presence <- return $ userId `elem` (albumShares album)
          case ownerPresence || presence of
            True -> do
              entities <- runDB $ selectList [UserId !=. (albumOwner album)] [Desc UserName]
              users <- return $ map (\u -> (userName $ entityVal u, entityKey u)) entities
              (albumSettingsWidget, enctype) <- generateFormPost $ albumSettingsForm album albumId users
              defaultLayout $ do
                setTitle "Eidolon :: Album Settings"
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
          ownerPresence <- return (userId == ownerId)
          presence <- return $ userId `elem` (albumShares album)
          case ownerPresence || presence of
            True -> do
              entities <- runDB $ selectList [UserId !=. (albumOwner album)] [Desc UserName]
              users <- return $ map (\u -> (userName $ entityVal u, entityKey u)) entities
              ((result, albumSettingsWidget), enctype) <- runFormPost $ albumSettingsForm album albumId users
              case result of
                FormSuccess temp -> do
                  aId <- runDB $ update albumId 
                    [ AlbumTitle =. albumTitle temp
                    , AlbumShares =. albumShares temp
                    , AlbumSamplePic =. albumSamplePic temp
                    ]
                  setMessage "Album settings changed succesfully"
                  redirect $ AlbumR albumId
                _ -> do
                  setMessage "There was an error while changing the settings"
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

albumSettingsForm :: Album -> AlbumId -> [(Text, UserId)]-> Form Album
albumSettingsForm album albumId users = renderDivs $ Album
  <$> areq textField "Title" (Just $ albumTitle album)
  <*> pure (albumOwner album)
  <*> areq (userField users) "Share this album with" (Just $ albumShares album)
  <*> pure (albumContent album)
  <*> aopt (selectField media) "Sample picture" (Just $ albumSamplePic album)
  where
    media = do
      entities <- runDB $ selectList [MediumAlbum ==. albumId] [Desc MediumTitle]
      optionsPairs $ map (\med -> (mediumTitle $ entityVal med, mediumThumb (entityVal med))) entities
--    users = do
--      entities <- runDB $ selectList [UserId !=. (albumOwner album)] [Desc UserName]
--      return $ map (\u -> (userName $ entityVal u, entityKey u)) entities

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
                setTitle $ toHtml ("Eidolon :: Delete album" `T.append` (albumTitle album))
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
                  -- remove album reference from user
                  albumList <- return $ userAlbums owner
                  newAlbumList <- return $ removeItem albumId albumList
                  runDB $ update ownerId [UserAlbums =. newAlbumList]
                  -- delete album content
                  mapM (\a -> runDB $ delete a) (albumContent album)
                  -- delete album
                  runDB $ delete albumId
                  -- delete files
                  liftIO $ removeDirectoryRecursive $ "static" </> "data" </> (T.unpack $ extractKey userId) </> (T.unpack $ extractKey albumId)
                  -- outro
                  setMessage "Album deleted succesfully"
                  redirect $ HomeR
                _ -> do
                  setMessage "You must confirm the deletion"
                  redirect $ AlbumSettingsR albumId
            _ -> do
              setMessage "You must own this album to delete it"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to delete albums"
          redirect $ LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect $ HomeR
