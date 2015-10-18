--  eidolon -- A simple gallery in Haskell and Yesod
--  Copyright (C) 2015  Amedeo Molnár
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Affero General Public License as published
--  by the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Affero General Public License for more details.
--
--  You should have received a copy of the GNU Affero General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Handler.AdminAlbumSettings where

import Import
import Handler.Commons
import qualified Data.Text as T
import qualified Data.List as L
import Data.Maybe
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
      redirect route

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
          redirect AdminR
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route

getAdminAlbumSettingsR :: AlbumId -> Handler Html
getAdminAlbumSettingsR albumId = do
  adminCheck <- loginIsAdmin
  case adminCheck of
    Right _ -> do
      tempAlbum <- runDB $ get albumId
      case tempAlbum of
        Just album -> do
          entities <- runDB $ selectList [UserId !=. albumOwner album] [Desc UserName]
          let users = map (\u -> (userName $ entityVal u, entityKey u)) entities
          (adminAlbumSettingsWidget, enctype) <- generateFormPost $ adminAlbumSettingsForm album albumId users
          formLayout $ do
            setTitle "Administration: Album settings"
            $(widgetFile "adminAlbumSet")
        Nothing -> do
          setMessage "This album does not exist"
          redirect AdminR
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route

postAdminAlbumSettingsR :: AlbumId -> Handler Html
postAdminAlbumSettingsR albumId = do
  adminCheck <- loginIsAdmin
  case adminCheck of
    Right _ -> do
      tempAlbum <- runDB $ get albumId
      case tempAlbum of
        Just album -> do
          entities <- runDB $ selectList [UserId !=. albumOwner album] [Desc UserName]
          let users = map (\u -> (userName $ entityVal u, entityKey u)) entities
          ((res, _), _) <- runFormPost $ adminAlbumSettingsForm album albumId users
          case res of
            FormSuccess temp -> do
              width <- getThumbWidth $ Just $ L.tail $ fromMaybe "a" $ albumSamplePic temp
              _ <- runDB $ update albumId
                [ AlbumTitle =. albumTitle temp
                , AlbumShares =. albumShares temp
                , AlbumSamplePic =. albumSamplePic temp
                , AlbumSampleWidth =. width
                ]
              putIndexES (ESAlbum albumId temp)
              setMessage "Album settings changed successfully"
              redirect AdminR
            _ -> do
              setMessage "There was an error while changing the settings"
              redirect $ AdminAlbumSettingsR albumId
        Nothing -> do
          setMessage "This album does not exist"
          redirect AdminR
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route

adminAlbumSettingsForm :: Album -> AlbumId -> [(Text, UserId)] -> Form Album
adminAlbumSettingsForm album albumId users = renderDivs $ Album
  <$> areq textField "Title" (Just $ albumTitle album)
  <*> pure (albumOwner album)
  <*> areq (userField users) "This album shared with" (Just $ albumShares album)
  <*> pure (albumContent album)
  <*> aopt (selectField media) "Sample picture" (Just $ albumSamplePic album)
  <*> pure 230
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
          let ownerId = albumOwner album
          owner <- runDB $ getJust ownerId
          let albumList = userAlbums owner
          let newAlbumList = removeItem albumId albumList
          runDB $ update ownerId [UserAlbums =. newAlbumList]
          -- delete album content and its comments
          _ <- mapM (\a -> do
            -- delete files
            medium <- runDB $ getJust a
            liftIO $ removeFile (normalise $ L.tail $ mediumPath medium)
            liftIO $ removeFile (normalise $ L.tail $ mediumThumb medium)
            -- delete comments
            commEnts <- runDB $ selectList [CommentOrigin ==. a] []
            _ <- mapM (\c -> do
              -- delete comment from elasticsearch
              children <- runDB $ selectList [CommentParent ==. (Just $ entityKey c)] []
              _ <- mapM (\child -> do
                -- delete comment children from elasticsearch and db
                deleteIndexES (ESComment (entityKey child) (entityVal child))
                runDB $ delete $ entityKey child
                ) children
              deleteIndexES (ESComment (entityKey c) (entityVal c))
              runDB $ delete $ entityKey c) commEnts
            -- delete album from elasticsearch
            deleteIndexES (ESAlbum albumId album)
            -- delete album database entry
            runDB $ delete a
            ) (albumContent album)
          -- delete from elasticsearch
          deleteIndexES (ESAlbum albumId album)
          -- delete album
          runDB $ delete albumId
          -- delete files
          liftIO $ removeDirectoryRecursive $ "static" </> "data" </> T.unpack (extractKey ownerId) </> T.unpack (extractKey albumId)
          -- outro
          setMessage "Album deleted successfully"
          redirect AdminR
        Nothing -> do
          setMessage "This album dies not exist"
          redirect AdminR
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route
