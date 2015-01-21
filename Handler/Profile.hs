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

getProfileR :: UserId -> Handler Html
getProfileR ownerId = do
  tempOwner <- runDB $ get ownerId
  case tempOwner of
    Just owner -> do
      ownerSlug <- lift $ pure $ userSlug owner
      userAlbs <- runDB $ selectList [AlbumOwner ==. ownerId] [Asc AlbumTitle]
      allAlbs <- runDB $ selectList [] [Asc AlbumTitle]
      almostAlbs <- mapM (\alb -> do
        case ownerId `elem` (albumShares $ entityVal alb) of
          True -> return $ Just alb
          False -> return Nothing
        ) allAlbs
      sharedAlbs <- return $ removeItem Nothing almostAlbs
      recentMedia <- (runDB $ selectList [MediumOwner ==. ownerId] [Desc MediumTime])
      msu <- lookupSession "userId"
      presence <- case msu of
        Just tempUserId -> do
          userId <- lift $ pure $ getUserIdFromText tempUserId
          return (userId == ownerId)
        Nothing ->
          return False
      defaultLayout $ do
        setTitle $ toHtml ("Eidolon :: " `T.append` (userSlug owner) `T.append` "'s profile")
        $(widgetFile "profile")
    Nothing -> do
      setMessage "This profile does not exist"
      redirect $ HomeR

getUserR :: Text -> Handler Html
getUserR ownerName = do
  tempOwner <- runDB $ selectFirst [UserName ==. ownerName] []
  case tempOwner of
    Just (Entity ownerId _) ->
      getProfileR ownerId
    Nothing -> do
      setMessage "This user does not exist"
      redirect $ HomeR
