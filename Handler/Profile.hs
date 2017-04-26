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

module Handler.Profile where

import Import
import Data.Maybe
import qualified Data.Text as T
import System.FilePath
import Yesod.RssFeed
import Yesod.AtomFeed

getProfileR :: UserId -> Handler Html
getProfileR ownerId = do
  tempOwner <- runDB $ get ownerId
  case tempOwner of
    Just owner -> do
      let ownerSlug = userSlug owner
      userAlbs <- runDB $ selectList [AlbumOwner ==. ownerId] [Asc AlbumTitle]
      allAlbs <- runDB $ selectList [] [Asc AlbumTitle]
      almostAlbs <- mapM (\alb ->
        if
          ownerId `elem` albumShares (entityVal alb)
          then return $ Just alb
          else return Nothing
        ) allAlbs
      let sharedAlbs = catMaybes almostAlbs
      recentMedia <- runDB $ selectList [MediumOwner ==. ownerId] [Desc MediumTime]
      -- msu <- lookupSession "userId"
      musername <- maybeAuthId
      presence <- case musername of
        Just username ->
          return (username == userName owner)
        Nothing ->
          return False
      defaultLayout $ do
        setTitle $ toHtml ("Eidolon :: " `T.append` ownerSlug `T.append` "'s profile")
        rssLink (UserFeedRssR ownerId) (ownerSlug `T.append` "'s feed")
        atomLink (UserFeedAtomR ownerId) (ownerSlug `T.append` "'s feed")
        $(widgetFile "profile")
    Nothing -> do
      setMessage "This profile does not exist"
      redirect HomeR

getUserR :: Text -> Handler Html
getUserR ownerName = do
  tempOwner <- runDB $ getBy $ UniqueUser ownerName
  case tempOwner of
    Just (Entity ownerId _) ->
      getProfileR ownerId
    Nothing -> do
      setMessage "This user does not exist"
      redirect HomeR
