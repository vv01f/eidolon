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

module Handler.NewAlbum where

import Import
import Data.Text
import System.Directory
import System.FilePath

getNewAlbumR :: Handler Html
getNewAlbumR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- lift $ pure $ getUserIdFromText tempUserId
      (albumWidget, enctype) <- generateFormPost (albumForm userId)
      formLayout $ do
        setTitle "Eidolon :: Create new Album"
        $(widgetFile "newAlbum")
    Nothing -> do
      setMessage "You need to be logged in"
      redirect LoginR

postNewAlbumR :: Handler Html
postNewAlbumR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      let userId = getUserIdFromText tempUserId
      ((result, _), _) <- runFormPost (albumForm userId)
      case result of
        FormSuccess album -> do
          -- Put album in Database
          albumId <- runDB $ insert album
          -- add album reference in user
          user <- runDB $ getJust userId
          let albumList = userAlbums user
          let newAlbumList = albumId : albumList
          runDB $ update userId [UserAlbums =. newAlbumList]
          -- create folder
          liftIO $ createDirectory $ "static" </> "data" </> unpack (extractKey userId) </> unpack (extractKey albumId)
          -- outro
          setMessage "Album successfully created"
          redirect $ ProfileR userId
        _ -> do
          setMessage "There was an error creating the album"
          redirect NewAlbumR
    Nothing -> do
      setMessage "You must be logged in to create albums"
      redirect LoginR

albumForm :: UserId -> Form Album
albumForm userId = renderDivs $ Album
  <$> areq textField "Title" Nothing
  <*> pure userId
  <*> pure []
  <*> pure []
  <*> pure Nothing
  <*> pure 230
