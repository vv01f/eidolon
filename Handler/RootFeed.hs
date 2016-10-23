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

module Handler.RootFeed where

import Import
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.List as L
import Data.Maybe
import Data.Time
import Text.Shakespeare.Text (stext)
import Yesod.Feed
import Yesod.RssFeed
import Yesod.AtomFeed
import System.FilePath
import qualified System.Posix as P

getCommentFeedRssR :: MediumId -> Handler RepRss
getCommentFeedRssR mId = do
  feed <- commentFeedBuilder mId
  rssFeed feed

getCommentFeedAtomR :: MediumId -> Handler RepAtom
getCommentFeedAtomR mId = do
  feed <- commentFeedBuilder mId
  atomFeed feed

commentFeedBuilder :: MediumId -> Handler (Feed (Route App))
commentFeedBuilder mId = do
  medium <- runDB $ get404 mId
  owner <- runDB $ getJust $ mediumOwner medium
  cs <- runDB $ selectList [CommentOrigin ==. mId] [Desc CommentTime, LimitTo 100]
  time <- case cs of
    x:_ -> return $ commentTime $ entityVal x
    [] -> liftIO getCurrentTime
  es <- mapM commentToEntry cs
  route <- fromJust <$> getCurrentRoute
  return $ Feed
    { feedTitle = "Eidolon :: Newest Comments on " `T.append` (mediumTitle medium)
    , feedLinkSelf = route
    , feedLinkHome = MediumR mId
    , feedAuthor = userSlug owner
    , feedDescription = [shamlet|
        These are the latest comments on the medium #{mediumTitle medium} by #{userSlug owner}
      |]
    , feedLanguage = "en"
    , feedUpdated = time
    , feedLogo = Just (StaticR $ StaticRoute (drop 2 $ map T.pack $ splitDirectories $ mediumThumb medium) [], unTextarea $ fromMaybe (Textarea "") $ mediumDescription medium)
    , feedEntries = es
    }

commentToEntry :: Entity Comment -> Handler (FeedEntry (Route App))
commentToEntry c = do
  author <- runDB $ get $ commentAuthor $ entityVal c
  let slug = fromMaybe "" $ userSlug <$> author
  return FeedEntry
    { feedEntryLink = MediumR (commentOrigin $ entityVal c)
    , feedEntryUpdated = (commentTime $ entityVal c)
    , feedEntryTitle = LT.toStrict $ [stext|#{slug} wrote:|]
    , feedEntryContent = [shamlet|#{commentContent $ entityVal c}|]
    , feedEntryEnclosure = Nothing
    }

getAlbumFeedRssR :: AlbumId -> Handler RepRss
getAlbumFeedRssR aId = do
  feed <- albumFeedBuilder aId
  rssFeed feed

getAlbumFeedAtomR :: AlbumId -> Handler RepAtom
getAlbumFeedAtomR aId = do
  feed <- albumFeedBuilder aId
  atomFeed feed

albumFeedBuilder :: AlbumId -> Handler (Feed (Route App))
albumFeedBuilder aId = do
  album <- runDB $ get404 aId
  owner <- runDB $ getJust $ albumOwner album
  ms <- runDB $ selectList [MediumAlbum ==. aId] [Desc MediumTime, LimitTo 100]
  time <- case ms of
    x:_ -> return $ mediumTime $ entityVal x
    [] -> liftIO getCurrentTime
  es <- mapM mediumToEntry ms
  route <- fromJust <$> getCurrentRoute
  return Feed
    { feedTitle = "Eidolon :: Newest media in " `T.append` (albumTitle album)
    , feedLinkSelf = route
    , feedLinkHome = AlbumR aId
    , feedAuthor = userSlug owner
    , feedDescription = [shamlet|
        These are the latest media uploaded in #{userSlug owner}'s album #{albumTitle album}
      |]
    , feedLanguage = "en"
    , feedUpdated = time
    , feedLogo = Just (StaticR $ StaticRoute (drop 2 $ map T.pack $ splitDirectories $ fromMaybe "/static/img/album.jpg" $ albumSamplePic album) [], "")
    , feedEntries = es
    }

getNameFeedRssR :: Text -> Handler RepRss
getNameFeedRssR name = do
  Entity uId _ <- runDB $ getBy404 $ UniqueUser name
  getUserFeedRssR uId

getNameFeedAtomR :: Text -> Handler RepAtom
getNameFeedAtomR name = do
  Entity uId _ <- runDB $ getBy404 $ UniqueUser name
  getUserFeedAtomR uId

getUserFeedRssR :: UserId -> Handler RepRss
getUserFeedRssR uId = do
  feed <- userFeedBuilder uId
  rssFeed feed

getUserFeedAtomR :: UserId -> Handler RepAtom
getUserFeedAtomR uId = do
  feed <- userFeedBuilder uId
  atomFeed feed

userFeedBuilder :: UserId -> Handler (Feed (Route App))
userFeedBuilder uId = do
  user <- runDB $ get404 uId
  ms <- runDB $ selectList [MediumOwner ==. uId] [Desc MediumTime, LimitTo 100]
  time <- case ms of
    x:_ -> return $ mediumTime $ entityVal x
    [] -> liftIO getCurrentTime
  es <- mapM mediumToEntry ms
  route <- fromJust <$> getCurrentRoute
  return Feed
    { feedTitle = "Eidolon :: Newest media of " `T.append` (userSlug user)
    , feedLinkSelf = route
    , feedLinkHome = ProfileR uId
    , feedAuthor = userSlug user
    , feedDescription = [shamlet|
        These are the latest media uploaded by #{userSlug user} to Eidolon.
      |]
    , feedLanguage = "en"
    , feedUpdated = time
    , feedLogo = Nothing
    , feedEntries = es
    }

getRootFeedRssR :: Handler RepRss
getRootFeedRssR = do
  feed <- rootFeedBuilder
  rssFeed feed

getRootFeedAtomR :: Handler RepAtom
getRootFeedAtomR = do
  feed <- rootFeedBuilder
  atomFeed feed

rootFeedBuilder :: Handler (Feed (Route App))
rootFeedBuilder = do
  ms <- runDB $ selectList [] [Desc MediumTime, LimitTo 100]
  time <- case ms of
    x:_ -> return $ mediumTime $ entityVal x
    [] -> liftIO getCurrentTime
  es <- mapM mediumToEntry ms
  route <- fromJust <$> getCurrentRoute
  return Feed
    { feedTitle = "Eidolon :: Latest media"
    , feedLinkSelf = route
    , feedLinkHome = HomeR
    , feedAuthor = "Everyone"
    , feedDescription = [shamlet|
        These are the latest media uploaded to the Eidolon gallery service.
      |]
    , feedLanguage = "en"
    , feedUpdated = time
    , feedLogo = Nothing
    , feedEntries = es
    }

mediumToEntry :: MonadIO m => Entity Medium -> m (FeedEntry (Route App))
mediumToEntry ent = do
  size <- liftIO $ getSize $ L.tail $ mediumPreview $ entityVal ent
  return FeedEntry
    { feedEntryLink = MediumR (entityKey ent)
    , feedEntryUpdated = mediumTime (entityVal ent)
    , feedEntryTitle = mediumTitle (entityVal ent)
    , feedEntryContent = toHtml (fromMaybe (Textarea "") $ mediumDescription $ entityVal ent)
    , feedEntryEnclosure = Just $ EntryEnclosure
        (StaticR $ StaticRoute (drop 2 $ map T.pack $ splitDirectories $ mediumPreview $ entityVal ent) [])
        size
        "image/jpeg"
    }

getSize :: FilePath -> IO Int
getSize path = do
  stat <- P.getFileStatus path
  P.COff raw <- return $ P.fileSize stat
  return $ (fromIntegral raw :: Int)
