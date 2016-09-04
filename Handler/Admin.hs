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

module Handler.Admin where

import Import
import Handler.Commons
-- import Database.Bloodhound

getAdminR :: Handler Html
getAdminR = do
  adminCheck <- loginIsAdmin
  case adminCheck of
    Right _ ->
      defaultLayout $ do
        setTitle "Administration: Menu"
        $(widgetFile "adminBase")
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route

-- getAdminSearchReloadR :: Handler Html
-- getAdminSearchReloadR = do
--   adminCheck <- loginIsAdmin
--   case adminCheck of
--     Right _ -> do
--       _ <- runBH' $ deleteIndex $ IndexName "_all"
--       users    <- runDB $ selectList [] [Asc UserId]
--       albums   <- runDB $ selectList [] [Asc AlbumId]
--       media    <- runDB $ selectList [] [Asc MediumId]
--       comments <- runDB $ selectList [] [Asc CommentId]
--       mapM_ (\ u -> putIndexES $ ESUser (entityKey u) (entityVal u))    users
--       mapM_ (\ u -> putIndexES $ ESAlbum (entityKey u) (entityVal u))   albums
--       mapM_ (\ u -> putIndexES $ ESMedium (entityKey u) (entityVal u))  media
--       mapM_ (\ u -> putIndexES $ ESComment (entityKey u) (entityVal u)) comments
--       setMessage "search indices repopulated"
--       redirect AdminR
--     Left (msg, route) -> do
--       setMessage msg
--       redirect route
