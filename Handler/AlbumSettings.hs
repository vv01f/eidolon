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

module Handler.AlbumSettings where

import Import
import Handler.Commons
import qualified Data.Text as T
import Data.Maybe
import System.Directory
import System.FilePath
import qualified Data.List as L

getAlbumSettingsR :: AlbumId -> Handler Html
getAlbumSettingsR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of
    Just album -> do
      let ownerId = albumOwner album
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          let userId = getUserIdFromText tempUserId
          let ownerPresence = userId == ownerId
          let presence = userId `elem` (albumShares album)
          case ownerPresence || presence of
            True -> do
              entities <- runDB $ selectList [UserId !=. (albumOwner album)] [Desc UserName]
              let users = map (\u -> (userName $ entityVal u, entityKey u)) entities
              (albumSettingsWidget, enctype) <- generateFormPost $
                renderBootstrap3 BootstrapBasicForm $
                albumSettingsForm album albumId users
              formLayout $ do
                setTitle "Eidolon :: Album Settings"
                $(widgetFile "albumSettings")
            False -> do
              setMessage "You must own this album to change its settings"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to change settings"
          redirect LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect HomeR

postAlbumSettingsR :: AlbumId -> Handler Html
postAlbumSettingsR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of
    Just album -> do
      let ownerId = albumOwner album
      owner <- runDB $ getJust ownerId
      let ownerName = userName owner
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          let userId = getUserIdFromText tempUserId
          let ownerPresence = userId == ownerId
          let presence = userId `elem` albumShares album
          if
            ownerPresence || presence
            then do
              entities <- runDB $ selectList [UserId !=. (albumOwner album)] [Desc UserName]
              let users = map (\u -> (userName $ entityVal u, entityKey u)) entities
              ((result, _), _) <- runFormPost $
                renderBootstrap3 BootstrapBasicForm $
                albumSettingsForm album albumId users
              case result of
                FormSuccess temp -> do
                  let newShares = L.sort $ albumShares temp
                  let oldShares = L.sort $ albumShares album
                  _ <- if
                    newShares /= oldShares
                    then do
                      link <- ($ AlbumR albumId) <$> getUrlRender
                      let rcptIds = L.nub $ newShares L.\\ oldShares
                      mapM (\uId -> do
                        -- update userAlbums
                        user <- runDB $ getJust uId
                        let oldAlbs = userAlbums user
                        let newAlbs = albumId : oldAlbs
                        _ <- runDB $ update uId [UserAlbums =. newAlbs]
                        -- send notification
                        let addr = userEmail user
                        sendMail addr "A new album was shared with you" $
                          [shamlet|
                            <h1>Hello #{userSlug user}!
                            <p>#{ownerName} was so kind to share his album #{albumTitle album} with you.
                            <p>You can find it
                              <a href=#{link}>
                                here
                              .
                            |]
                        ) rcptIds
                    else  do
                      return [()]
                      -- nothing to do here
                  sample <- runDB $ selectFirst
                    [MediumThumb ==. (fromMaybe "" $ albumSamplePic temp)] []
                  let width = if isNothing sample then 230 else mediumThumbWidth $ entityVal $ fromJust sample
                  _ <- runDB $ update albumId 
                    [ AlbumTitle =. albumTitle temp
                    , AlbumShares =. newShares
                    , AlbumSamplePic =. albumSamplePic temp
                    , AlbumSampleWidth =. width
                    ]
                  setMessage "Album settings changed succesfully"
                  redirect $ AlbumR albumId
                _ -> do
                  setMessage "There was an error while changing the settings"
                  redirect $ AlbumSettingsR albumId
            else do
              setMessage "You must own this album to change its settings"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to change settings"
          redirect LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect HomeR

albumSettingsForm :: Album -> AlbumId -> [(Text, UserId)]-> AForm Handler Album
albumSettingsForm album albumId users = Album
  <$> areq textField (bfs ("Title" :: T.Text)) (Just $ albumTitle album)
  <*> pure (albumOwner album)
  <*> areq (userField users) (bfs ("Share this album with" :: T.Text)) (Just $ albumShares album)
  <*> pure (albumContent album)
  <*> aopt (selectField media) (bfs ("Sample picture" :: T.Text)) (Just $ albumSamplePic album)
  <*> pure 230
  <*  bootstrapSubmit ("Change settings" :: BootstrapSubmit Text)
  where
    media = do
      entities <- runDB $ selectList [MediumAlbum ==. albumId] [Asc MediumTitle]
      optionsPairs $ map (\med -> (mediumTitle $ entityVal med, mediumThumb (entityVal med))) entities

getAlbumDeleteR :: AlbumId -> Handler Html
getAlbumDeleteR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of
    Just album -> do
      let ownerId = albumOwner album
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          let userId = getUserIdFromText tempUserId
          if
            userId == ownerId
            then do
              formLayout $ do
                setTitle $ toHtml ("Eidolon :: Delete album" `T.append` (albumTitle album))
                $(widgetFile "albumDelete")
            else do
              setMessage "You must own this album to delete it"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to delete albums"
          redirect LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect HomeR

postAlbumDeleteR :: AlbumId -> Handler Html
postAlbumDeleteR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of
    Just album -> do
      let ownerId = albumOwner album
      owner <- runDB $ getJust ownerId
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          let userId = getUserIdFromText tempUserId
          if 
            userId == ownerId
            then do
              confirm <- lookupPostParam "confirm"
              case confirm of
                Just "confirm" -> do
                  -- remove album reference from user
                  let albumList = userAlbums owner
                  let newAlbumList = removeItem albumId albumList
                  runDB $ update ownerId [UserAlbums =. newAlbumList]
                  -- delete album content and its comments
                  _ <- mapM (\a -> do
                    -- delete files
                    medium <- runDB $ getJust a
                    liftIO $ removeFile (normalise $ L.tail $ mediumPath medium)
                    liftIO $ removeFile (normalise $ L.tail $ mediumThumb medium)
                    liftIO $ removeFile (normalise $ L.tail $ mediumPreview medium)
                    -- delete comments
                    commEnts <- runDB $ selectList [CommentOrigin ==. a] []
                    _ <- mapM (\c -> do
                      children <- runDB $ selectList [CommentParent ==. (Just $ entityKey c)] []
                      _ <- mapM (\child -> do
                        -- delete comment children from elasticsearch and db
                        runDB $ delete $ entityKey child
                        ) children
                      -- delete comment from elasticsearch
                      runDB $ delete $ entityKey c) commEnts
                    runDB $ delete a
                    ) (albumContent album)
                  -- delete album
                  runDB $ delete albumId
                  -- delete files
                  liftIO $ removeDirectoryRecursive $ "static" </> "data" </> T.unpack (extractKey userId) </> T.unpack (extractKey albumId)
                  -- outro
                  setMessage "Album deleted succesfully"
                  redirect HomeR
                _ -> do
                  setMessage "You must confirm the deletion"
                  redirect $ AlbumSettingsR albumId
            else do
              setMessage "You must own this album to delete it"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to delete albums"
          redirect LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect HomeR
