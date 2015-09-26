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

module Handler.AdminComments where

import Import
import Handler.Commons
import Data.Time
-- import System.Locale

getAdminCommentR :: Handler Html
getAdminCommentR = do
  adminCheck <- loginIsAdmin
  case adminCheck of
    Right _ -> do
      media <- runDB $ selectList [] [Desc MediumTime]
      comments <- runDB $ selectList [CommentParent ==. Nothing] [Desc CommentTime]
      replies <- runDB $ selectList [CommentParent !=. Nothing] [Desc CommentTime]
      formLayout $ do
        setTitle "Administration: Comments"
        $(widgetFile "adminComments")
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route

getAdminCommentDeleteR :: CommentId -> Handler Html
getAdminCommentDeleteR commentId = do
  adminCheck <- loginIsAdmin
  case adminCheck of
    Right _ -> do
      tempComment <- runDB $ get commentId
      case tempComment of
        Just _ -> do
          -- delete comment children
          children <- runDB $ selectList [CommentParent ==. Just commentId] []
          _ <- mapM (runDB . delete . entityKey) children
          -- delete comment itself
          runDB $ delete commentId
          setMessage "Comment deleted succesfully"
          redirect AdminR
        Nothing -> do
          setMessage "This comment does not exist"
          redirect AdminR
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route
