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

module Handler.ProfileDelete where

import Import
import Handler.Commons
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.List as L
import System.Directory
import System.FilePath

getProfileDeleteR :: UserId -> Handler Html
getProfileDeleteR userId = do
  checkRes <- profileCheck userId
  case checkRes of
    Right user ->
      formLayout $ do
        setTitle "Eidolon :: Delete user profile"
        $(widgetFile "profileDelete")
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route

postProfileDeleteR :: UserId -> Handler Html
postProfileDeleteR userId = do
  checkRes <- profileCheck userId
  case checkRes of
    Right user -> do
      confirm <- lookupPostParam "confirm"
      case confirm of
        Just "confirm" -> do
          let albumList = userAlbums user
          _ <- mapM (\albumId -> do
            album <- runDB $ getJust albumId
            when (albumOwner album == userId) $ do
              let mediaList = albumContent album
              _ <- mapM (\med -> do
                commEnts <- runDB $ selectList [CommentOrigin ==. med] []
                _ <- mapM (\ent -> do
                  children <- runDB $ selectList [CommentParent ==. (Just $ entityKey ent)] []
                  _ <- mapM (\child -> do
                    -- delete comment children
                    deleteIndexES $ ESComment (entityKey child) (entityVal child)
                    runDB $ delete $ entityKey child
                    ) children
                  -- delete comment
                  deleteIndexES $ ESComment (entityKey ent) (entityVal ent)
                  runDB $ delete $ entityKey ent) commEnts
                medium <- runDB $ getJust med
                liftIO $ removeFile (normalise $ L.tail $ mediumPath medium)
                liftIO $ removeFile (normalise $ L.tail $ mediumThumb medium)
                liftIO $ removeFile (normalise $ L.tail $ mediumPreview medium)
                deleteIndexES (ESMedium med medium)
                runDB $ delete med
                ) mediaList
              deleteIndexES $ ESAlbum albumId album
              runDB $ delete albumId
            ) albumList
          deleteIndexES $ ESUser userId user
          runDB $ delete userId
          liftIO $ removeDirectoryRecursive $ "static" </> "data" </> T.unpack (extractKey userId)
          deleteSession "userId"
          setMessage "User deleted successfully"
          redirect HomeR
        _ -> do
          setMessage "You must confirm the deletion"
          redirect $ ProfileSettingsR userId
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route
