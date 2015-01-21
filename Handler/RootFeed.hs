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
import Yesod
import Yesod.Static
import qualified Data.Text as T
import Data.Maybe
import Data.Time
import Data.Either.Combinators
import Blaze.ByteString.Builder

data FeedParameters = Parameters
  { pTitle :: T.Text
  , pImage :: T.Text
  , pLink :: Route App
  }

nsAtom :: T.Text
nsAtom = "http://www.w3.org/2005/Atom"

class RepFeed c where
  renderFeed :: FeedParameters -> Either [Entity Comment] [Entity Medium] -> Handler c

newtype RepAtom = RepAtom Content
  deriving (ToContent)

instance ToTypedContent RepAtom where
  toTypedContent =
    TypedContent typeAtom . withXmlDecl . toContent

withXmlDecl :: Content -> Content
withXmlDecl (ContentBuilder b _) =
  flip ContentBuilder Nothing $
  fromByteString "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n" `mappend`
  b
withXmlDecl c = c

instance RepFeed RepAtom where
  renderFeed params items = do
    image <- return $ pImage params
    url <- getUrlRender
    tz <- liftIO getCurrentTimeZone
    links <- case items of
      Left commEnts ->
        return $ Left $ map (\ent -> (entityKey ent, url $ MediumR $ commentOrigin $ entityVal ent)) commEnts
      Right mediaEnts ->
        return $ Right $ map (\ent -> (entityKey ent, url $ MediumR $ entityKey ent)) mediaEnts
    return $ RepAtom $ toContent $ 
               [xhamlet|$newline always
                 <feed version="1.0"
                       xmlns=#{nsAtom}>
                     <title>#{pTitle params}
                     <link rel="alternate"
                           type="text/html"
                           href=#{url $ pLink params}
                           >
                     <id>#{url $ pLink params}
                     $if not (T.null image)
                         <link rel="icon"
                               href=#{image}
                               >
                     $case items
                       $of Right media
                         $forall (Entity mediumId medium) <- media
                           <entry xml:lang="en">
                             <title>#{mediumTitle medium}
                             <link rel="alternate"
                               type="text/html"
                               href=#{fromJust $ lookup mediumId $ fromRight [] links}
                               >
                             <id>#{fromJust $ lookup mediumId $ fromRight [] links}
                             <published>#{iso8601 $ utcToZonedTime tz $ mediumTime medium}
                             <summary>#{mediumDescription medium}
                             <link rel="icon"
                               href=#{url $ StaticR $ mediumStaticThumbRoute medium}
                               >
                             <link rel="enclosure"
                               type=#{mediumMime medium}
                               href=#{url $ StaticR $ mediumStaticImageRoute medium}
                               >
                       $of Left comments
                         $forall (Entity commentId comment) <- comments
                           <entry xml:lang="en">
                             <title>#{fromJust $ commentAuthorSlug comment} commented
                             <link rel="alternate"
                               type="text/html"
                               href=#{fromJust $ lookup commentId $ fromLeft [] links}
                               >
                             <id>#{fromJust $ lookup commentId $ fromLeft [] links}
                             <published>#{iso8601 $ utcToZonedTime tz $ commentTime comment}
                             <summary>#{commentContent comment}
                    |] url

newtype RepRss = RepRss Content
    deriving (ToContent)

instance ToTypedContent RepRss where
    toTypedContent =
        TypedContent typeRss .
        withXmlDecl . toContent
      
instance RepFeed RepRss where
  renderFeed params items = do
    url <- getUrlRender
    let image = pImage params
    links <- case items of
      Left commEnts ->
        return $ Left $ map (\ent -> (entityKey ent, url $ MediumR $ commentOrigin $ entityVal ent)) commEnts
      Right mediaEnts ->
        return $ Right $ map (\ent -> (entityKey ent, url $ MediumR $ entityKey ent)) mediaEnts
    return $ RepRss $ toContent $ 
               [xhamlet|$newline always
                 <rss version="2.0"
                      xmlns:atom=#{nsAtom}>
                   <channel>
                     <title>#{pTitle params}
                     <link>#{url $ pLink params}
                     $if not (T.null image)
                       <image>
                         <url>#{image}
                     $case items
                       $of Right media
                         $forall (Entity mediumId medium) <- media
                           <item>
                             <title>#{mediumTitle medium}
                             <link>#{fromJust $ lookup mediumId $ fromRight [] links}
                             <language>en
                             <description>#{mediumDescription medium}
                             <guid isPermaLink="true">#{fromJust $ lookup mediumId $ fromRight [] links}
                             <pubDate>#{rfc822 $ mediumTime medium}
                             <image>
                               <url>#{fromJust $ lookup mediumId $ fromRight [] links}
                             <enclosure type=#{mediumMime medium}
                                        url=#{url $ StaticR $ mediumStaticImageRoute medium}>
                       $of Left comments
                         $forall (Entity commentId comment) <- comments
                           <item>
                             <title>#{fromJust $ commentAuthorSlug comment} commented
                             <link>#{fromJust $ lookup commentId $ fromLeft [] links}
                             <language>en
                             <description>#{commentContent comment}
                             <pubdate>#{rfc822 $ commentTime comment}
                     |] url

getRootFeedAtomR :: Handler RepAtom
getRootFeedAtomR = getRootFeedR

getRootFeedRssR :: Handler RepRss
getRootFeedRssR = getRootFeedR

getRootFeedR :: RepFeed a => Handler a
getRootFeedR = do
  recentMedia <- runDB $ selectList [] [Desc MediumTime, LimitTo 100]
  renderFeed Parameters
    { pTitle = "Eidolon :: Latest media"
    , pLink  = HomeR
    , pImage = ""
    } (Right recentMedia)

getAlbumFeedAtomR :: AlbumId -> Handler RepAtom
getAlbumFeedAtomR = getAlbumFeedR

getAlbumFeedRssR :: AlbumId -> Handler RepRss
getAlbumFeedRssR = getAlbumFeedR

getAlbumFeedR :: RepFeed a => AlbumId -> Handler a
getAlbumFeedR albumId = do
  album <- runDB $ get404 albumId
  url <- getUrlRender
  recentMedia <- runDB $ selectList [MediumAlbum ==. albumId] [Desc MediumTime, LimitTo 100]
  renderFeed Parameters
    { pTitle = "Eidolon :: Latest media in album " `T.append` (albumTitle album)
    , pLink  = AlbumR albumId
    , pImage = url $ StaticR $ StaticRoute
        (drop 2 $ T.splitOn "/" $ T.pack $ fromMaybe "/static/img/album.jpg" $ albumSamplePic album) []
    } (Right recentMedia)

getCommentFeedAtomR :: MediumId -> Handler RepAtom
getCommentFeedAtomR = getCommentFeedR

getCommentFeedRssR :: MediumId -> Handler RepRss
getCommentFeedRssR = getCommentFeedR

getCommentFeedR :: RepFeed a => MediumId -> Handler a
getCommentFeedR mediumId = do
  medium <- runDB $ get404 mediumId
  url <- getUrlRender
  recentComments <- runDB $ selectList [CommentOrigin ==. mediumId] [Desc CommentTime, LimitTo 100]
  renderFeed Parameters
    { pTitle = "Eidolon :: Newest comments on " `T.append` (mediumTitle medium)
    , pLink  = MediumR mediumId
    , pImage = url $ StaticR $ mediumStaticThumbRoute medium
    } (Left recentComments)

getUserFeedAtomR :: UserId -> Handler RepAtom
getUserFeedAtomR = getUserFeedR

getUserFeedRssR :: UserId -> Handler RepRss
getUserFeedRssR = getUserFeedR

getUserFeedR :: RepFeed a => UserId -> Handler a
getUserFeedR userId = do
  user <- runDB $ get404 userId
  recentMedia <- runDB $ selectList [MediumOwner ==. userId] [Desc MediumTime, LimitTo 100]
  renderFeed Parameters
    { pTitle = "Eidolon :: Newest media of " `T.append` (userSlug user)
    , pLink  = ProfileR userId
    , pImage = ""
    } (Right recentMedia)
