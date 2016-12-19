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

module Handler.Album where

import Import
import qualified Data.Text as T
import Text.Blaze (toMarkup)
import System.FilePath
import Yesod.RssFeed
import Yesod.AtomFeed

getAlbumR :: AlbumId -> Handler Html
getAlbumR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of
    Just album -> do
      let ownerId = albumOwner album
      owner <- runDB $ getJust ownerId
      let ownerName = userName owner
      let ownerSlug = userSlug owner
      msu <- lookupSession "userId"
      presence <- case msu of
        Just tempUserId -> do
          let userId = getUserIdFromText tempUserId
          return $ (userId == ownerId) || (userId `elem` albumShares album)
        Nothing ->
          return False
--      media <- mapM (\a -> runDB $ getJust a) (albumContent album)
      media <- runDB $ selectList [MediumAlbum ==. albumId] [Desc MediumTime]
      defaultLayout $ do
        setTitle $ toHtml ("Eidolon :: Album " `T.append` albumTitle album)
        rssLink (AlbumFeedRssR albumId) $ "Album feed of album " `T.append` albumTitle album
        atomLink (AlbumFeedAtomR albumId) $ "Album feed of album " `T.append` albumTitle album
        $(widgetFile "album")
    Nothing -> do
      setMessage "This album does not exist"
      redirect HomeR

getBeautyAlbumR :: T.Text -> T.Text -> Handler Html
getBeautyAlbumR ownerName albumName = do
  tempOwner <- runDB $ getBy $ UniqueUser ownerName
  case tempOwner of
    Just (Entity ownerId _) -> do
      tempAlbum <- runDB $ selectFirst [AlbumTitle ==. albumName, AlbumOwner ==. ownerId] []
      case tempAlbum of
        Just (Entity albumId _) ->
          getAlbumR albumId
        Nothing -> do
          setMessage $ toMarkup $ "This user has no album named " `T.append` albumName
          redirect $ UserR ownerName
    Nothing -> do
      setMessage "This user does not exist"
      redirect HomeR
