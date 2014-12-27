module Handler.AdminAlbumSettings where

import Import
import Handler.Commons
import qualified Data.Text as T
import qualified Data.List as L
import System.FilePath
import System.Directory

getAdminAlbumsR :: Handler Html
getAdminAlbumsR = do
  adminCheck <- loginIsAdmin
  case adminCheck of
    Right _ -> do
      albums <- runDB $ selectList [] [Asc AlbumTitle]
      defaultLayout $ do
        setTitle "Administration: Albums"
        $(widgetFile "adminAlbums")
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect $ route

getAdminAlbumMediaR :: AlbumId -> Handler Html
getAdminAlbumMediaR albumId = do
  adminCheck <- loginIsAdmin
  case adminCheck of
    Right _ -> do
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
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect $ route

getAdminAlbumSettingsR :: AlbumId -> Handler Html
getAdminAlbumSettingsR albumId = do
  adminCheck <- loginIsAdmin
  case adminCheck of
    Right _ -> do
      tempAlbum <- runDB $ get albumId
      case tempAlbum of
        Just album -> do
          entities <- runDB $ selectList [UserId !=. (albumOwner album)] [Desc UserName]
          users <- return $ map (\u -> (userName $ entityVal u, entityKey u)) entities
          (adminAlbumSettingsWidget, enctype) <- generateFormPost $ adminAlbumSettingsForm album albumId users
          defaultLayout $ do
            setTitle "Administration: Album settings"
            $(widgetFile "adminAlbumSet")
        Nothing -> do
          setMessage "This album does not exist"
          redirect $ AdminR
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect $ route

postAdminAlbumSettingsR :: AlbumId -> Handler Html
postAdminAlbumSettingsR albumId = do
  adminCheck <- loginIsAdmin
  case adminCheck of
    Right _ -> do
      tempAlbum <- runDB $ get albumId
      case tempAlbum of
        Just album -> do
          entities <- runDB $ selectList [UserId !=. (albumOwner album)] [Desc UserName]
          users <- return $ map (\u -> (userName $ entityVal u, entityKey u)) entities
          ((res, adminAlbumSettingsWidget), enctype) <- runFormPost $ adminAlbumSettingsForm album albumId users
          case res of
            FormSuccess temp -> do
              aId <- runDB $ update albumId
                [ AlbumTitle =. albumTitle temp
                , AlbumShares =. albumShares temp
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
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect $ route

adminAlbumSettingsForm :: Album -> AlbumId -> [(Text, UserId)] -> Form Album
adminAlbumSettingsForm album albumId users = renderDivs $ Album
  <$> areq textField "Title" (Just $ albumTitle album)
  <*> pure (albumOwner album)
  <*> areq (userField users) "This album shared with" (Just $ albumShares album)
  <*> pure (albumContent album)
  <*> aopt (selectField media) "Sample picture" (Just $ albumSamplePic album)
  where
    media = do
      entities <- runDB $ selectList [MediumAlbum ==. albumId] [Asc MediumTitle]
      optionsPairs $ map (\med -> (mediumTitle $ entityVal med, mediumThumb (entityVal med))) entities

getAdminAlbumDeleteR :: AlbumId -> Handler Html
getAdminAlbumDeleteR albumId = do
  adminCheck <- loginIsAdmin
  case adminCheck of
    Right _ -> do
      tempAlbum <- runDB $ get albumId
      case tempAlbum of
        Just album -> do
          -- remove reference from owner
          ownerId <- return $ albumOwner album
          owner <- runDB $ getJust ownerId
          albumList <- return $ userAlbums owner
          newAlbumList <- return $ removeItem albumId albumList
          runDB $ update ownerId [UserAlbums =. newAlbumList]
          -- delete album content and its comments
          mapM (\a -> do
            -- delete files
            medium <- runDB $ getJust a
            liftIO $ removeFile (normalise $ L.tail $ mediumPath medium)
            liftIO $ removeFile (normalise $ L.tail $ mediumThumb medium)
            -- delete comments
            commEnts <- runDB $ selectList [CommentOrigin ==. a] []
            mapM (\ent -> runDB $ delete $ entityKey ent) commEnts
            -- delete album database entry
            runDB $ delete a
            ) (albumContent album)
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
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect $ route
