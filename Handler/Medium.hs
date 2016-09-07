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

module Handler.Medium where

import Import
import Data.Time
import Data.Maybe
import qualified Data.Text as T
import System.FilePath
import Yesod.Markdown
import Yesod.RssFeed
import Yesod.AtomFeed

getMediumR :: MediumId -> Handler Html
getMediumR mediumId = do
  tempMedium <- runDB $ get mediumId
  case tempMedium of
    Just medium -> do
      let ownerId = mediumOwner medium
      owner <- runDB $ getJust ownerId
      let ownerName = userName owner
      let albumId = mediumAlbum medium
      album <- runDB $ getJust albumId
      msu <- lookupSession "userId"
      userId <- case msu of
        Just tempUserId ->
          return $ Just $ getUserIdFromText tempUserId
        Nothing ->
          return Nothing
      userSl <- case userId of
        Just uId -> do
          u <- runDB $ getJust uId
          return $ Just $ userSlug u
        Nothing ->
          return Nothing
      let presence = userId == (Just ownerId) || userId == Just (albumOwner album)
      (commentWidget, enctype) <- generateFormPost $
        renderBootstrap3 BootstrapBasicForm $
        commentForm (fromJust userId) (fromJust userSl) mediumId Nothing
      comments <- runDB $ selectList
        [ CommentOrigin ==. mediumId
        , CommentParent ==. Nothing ]
        [ Desc CommentTime ]
      replies <- runDB $ selectList
        [ CommentOrigin ==. mediumId
        , CommentParent !=. Nothing ]
        [ Desc CommentTime ]
      let tr = StaticR $ StaticRoute (drop 2 $ map T.pack $ splitDirectories $ mediumThumb medium) []
      let pr = StaticR $ StaticRoute (drop 2 $ map T.pack $ splitDirectories $ mediumPreview medium) []
      let ir = StaticR $ StaticRoute (drop 2 $ map T.pack $ splitDirectories $ mediumPath medium) []
      defaultLayout $ do
        setTitle $ toHtml ("Eidolon :: Medium " `T.append` (mediumTitle medium))
        rssLink (CommentFeedRssR mediumId) $ "Comment feed of medium " `T.append` mediumTitle medium
        atomLink (CommentFeedAtomR mediumId) $ "Comment feed of medium " `T.append` mediumTitle medium
        $(widgetFile "medium")
    Nothing -> do
      setMessage "This image does not exist"
      redirect HomeR

postMediumR :: MediumId -> Handler Html
postMediumR mediumId = do
  tempMedium <- runDB $ get mediumId
  case tempMedium of
    Just medium -> do
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          let userId = getUserIdFromText tempUserId
          u <- runDB $ getJust userId
          let userSl = userSlug u
          ((res, _), _) <- runFormPost $
            renderBootstrap3 BootstrapBasicForm $
            commentForm userId userSl mediumId Nothing
          case res of
            FormSuccess temp -> do
              runDB $ insert_ temp
              --send mail to medium owner
              owner <- runDB $ getJust $ mediumOwner medium
              link <- ($ MediumR (commentOrigin temp)) <$> getUrlRender
              sendMail (userEmail owner) ((commentAuthorSlug temp) `T.append` " commented on your medium")
                [shamlet|
                  <h1>Hello #{userSlug owner}
                  <p>#{commentAuthorSlug temp} commented on your medium:
                  <p>#{commentContent temp}
                  <p>To follow the comment thread follow
                    <a href=#{link}>
                      this link
                    .
                  |]
              setMessage "Your Comment has been posted"
              redirect $ MediumR mediumId
            _ -> do
              setMessage "There has been an error whith your comment"
              redirect $ MediumR mediumId
        Nothing -> do
          setMessage "You need to be looged in to comment on media"
          redirect LoginR
    Nothing -> do
      setMessage "This image does not exist"
      redirect HomeR

commentForm :: UserId -> Text -> MediumId -> Maybe CommentId -> AForm Handler Comment
commentForm authorId authorSlug originId parentId = Comment
  <$> pure authorId
  <*> pure authorSlug
  <*> pure originId
  <*> pure parentId
  <*> lift (liftIO getCurrentTime)
  <*> areq markdownField (bfs ("Comment this medium" :: T.Text)) Nothing
  <*  bootstrapSubmit ("Post comment" :: BootstrapSubmit Text)

getCommentReplyR :: CommentId -> Handler Html
getCommentReplyR commentId = do
  tempComment <- runDB $ get commentId
  case tempComment of
    Just comment -> do
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          let userId = getUserIdFromText tempUserId
          u <- runDB $ getJust userId
          let userSl = userSlug u
          let mediumId = commentOrigin comment
          (replyWidget, enctype) <- generateFormPost $
            renderBootstrap3 BootstrapBasicForm $
            commentForm userId userSl mediumId (Just commentId)
          defaultLayout $ do
            setTitle "Eidolon :: Reply to comment"
            $(widgetFile "commentReply")
        Nothing -> do
          setMessage "You need to be logged in to comment on media"
          redirect LoginR
    Nothing -> do
      setMessage "This comment does not Exist"
      redirect HomeR

postCommentReplyR :: CommentId -> Handler Html
postCommentReplyR commentId = do
  tempComment <- runDB $ get commentId
  case tempComment of
    Just comment -> do
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          let userId = getUserIdFromText tempUserId
          u <- runDB $ getJust userId
          let userSl = userSlug u
          let mediumId = commentOrigin comment
          ((res, _), _) <- runFormPost $
            renderBootstrap3 BootstrapBasicForm $
            commentForm userId userSl mediumId (Just commentId)
          case res of
            FormSuccess temp -> do
              runDB $ insert_ temp
              --send mail to parent author
              parent <- runDB $ getJust $ fromJust $ commentParent temp
              parAuth <- runDB $ getJust $ commentAuthor parent
              link <- ($ MediumR (commentOrigin temp)) <$> getUrlRender
              sendMail (userEmail parAuth) ((commentAuthorSlug temp) `T.append` " replied to your comment")
                [shamlet|
                  <h1>Hello #{userSlug parAuth}
                  <p>#{commentAuthorSlug temp} replied to your comment:
                  #{commentContent temp}
                  <p>To see the comment thread follow
                    <a href=#{link}>
                      this link
                    .
                  |]
              --send mail to medium owner
              medium <- runDB $ getJust mediumId
              owner <- runDB $ getJust $ mediumOwner medium
              sendMail (userEmail owner) ((commentAuthorSlug temp) `T.append` " commented on your medium")
                [shamlet|
                  <h1>Hello #{userSlug owner}
                  <p>#{commentAuthorSlug temp} commented your medium with:
                  #{commentContent temp}
                  <p>To see the comment thread follow
                    <a href=#{link}>
                      this link
                    .
                  |]
              setMessage "Your reply has been posted"
              redirect $ MediumR mediumId
            _ -> do
              setMessage "There has been an error with your reply"
              redirect $ CommentReplyR commentId
        Nothing -> do
          setMessage "You need to be logged in to post replies"
          redirect LoginR
    Nothing -> do
      setMessage "This comment does not exist!"
      redirect HomeR

getCommentDeleteR :: CommentId -> Handler Html
getCommentDeleteR commentId = do
  tempComment <- runDB $ get commentId
  case tempComment of
    Just comment -> do
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          let userId = getUserIdFromText tempUserId
          if
            Just userId == Just (commentAuthor comment)
            then do
              defaultLayout $ do
                setTitle "Eidolon :: Delete comment"
                $(widgetFile "commentDelete")
            else do
              setMessage "You must be the author of this comment to delete it"
              redirect $ MediumR $ commentOrigin comment
        Nothing -> do
          setMessage "You must be logged in to delete comments"
          redirect LoginR
    Nothing -> do
      setMessage "This comment does not exist"
      redirect HomeR

postCommentDeleteR :: CommentId -> Handler Html
postCommentDeleteR commentId = do
  tempComment <- runDB $ get commentId
  case tempComment of
    Just comment -> do
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          let userId = getUserIdFromText tempUserId
          if
            Just userId == Just (commentAuthor comment)
            then do
              confirm <- lookupPostParam "confirm"
              case confirm of
                Just "confirm" -> do
                  -- delete comment children
                  childEnts <- runDB $ selectList [CommentParent ==. (Just commentId)] []
                  _ <- mapM (\ent -> do
                    -- delete comment children
                    runDB $ delete $ entityKey ent) childEnts
                  -- delete comment itself
                  runDB $ delete commentId
                  -- outro
                  setMessage "Your comment has been deleted"
                  redirect $ MediumR $ commentOrigin comment
                _ -> do
                  setMessage "You must confirm the deletion"
                  redirect $ MediumR $ commentOrigin comment
            else do
              setMessage "You must be the author of this comment to delete it"
              redirect $ MediumR $ commentOrigin comment
        Nothing -> do
          setMessage "You must be logged in to delete comments"
          redirect LoginR
    Nothing -> do
      setMessage "This comment does not exist"
      redirect HomeR
