module Handler.Medium where

import Import
import Data.Time
import Data.Maybe
import qualified Data.Text as T
import System.Locale
import Yesod.Markdown

getMediumR :: MediumId -> Handler Html
getMediumR mediumId = do
  tempMedium <- runDB $ get mediumId
  case tempMedium of
    Just medium -> do
      ownerId <- return $ mediumOwner medium
      owner <- runDB $ getJust ownerId
      ownerName <- return $ userName owner
      albumId <- return $ mediumAlbum medium
      album <- runDB $ getJust albumId
      msu <- lookupSession "userId"
      userId <- case msu of
        Just tempUserId -> do
          return $ Just $ getUserIdFromText tempUserId
        Nothing ->
          return Nothing
      userSl <- case userId of
        Just uId -> do
          u <- runDB $ getJust uId
          return $ Just $ userSlug u
        Nothing ->
          return Nothing
      presence <- return $ (userId == (Just ownerId) || userId == Just (albumOwner album))
      (commentWidget, enctype) <- generateFormPost $ commentForm userId userSl mediumId Nothing
      comments <- runDB $ selectList [CommentOrigin ==. mediumId, CommentParent ==. Nothing] [Desc CommentTime]
      replies <- runDB $ selectList [CommentOrigin ==. mediumId, CommentParent !=. Nothing] [Desc CommentTime]
      defaultLayout $ do
        setTitle $ toHtml ("Eidolon :: Medium " `T.append` (mediumTitle medium))
        $(widgetFile "medium")
    Nothing -> do
      setMessage "This image does not exist"
      redirect $ HomeR

postMediumR :: MediumId -> Handler Html
postMediumR mediumId = do
  tempMedium <- runDB $ get mediumId
  case tempMedium of
    Just medium -> do
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ Just $ getUserIdFromText tempUserId
          u <- runDB $ getJust $ fromJust userId
          userSl <- return $ Just $ userSlug u
          ((res, commentiwdget), enctype) <- runFormPost $ commentForm userId userSl mediumId Nothing
          case res of
            FormSuccess temp -> do
              cId <- runDB $ insert temp
              --send mail to medium owner
              owner <- runDB $ getJust $ mediumOwner medium
              link <- ($ MediumR (commentOrigin temp)) <$> getUrlRender
              sendMail (userEmail owner) ((fromJust $ commentAuthorSlug temp) `T.append` " commented on your medium")
                [shamlet|
                  <h1>Hello #{userSlug owner}
                  <p>#{fromJust $ commentAuthorSlug temp} commented on your medium:
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
      redirect $ HomeR

commentForm :: Maybe UserId -> Maybe Text -> MediumId -> Maybe CommentId -> Form Comment
commentForm authorId authorSlug originId parentId = renderDivs $ Comment
  <$> pure authorId
  <*> pure authorSlug
  <*> pure originId
  <*> pure parentId
  <*> lift (liftIO getCurrentTime)
  <*> areq markdownField "Comment this medium" Nothing

getCommentReplyR :: CommentId -> Handler Html
getCommentReplyR commentId = do
  tempComment <- runDB $ get commentId
  case tempComment of
    Just comment -> do
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ Just $ getUserIdFromText tempUserId
          u <- runDB $ getJust $ fromJust userId
          userSl <- return $ Just $ userSlug u
          mediumId <- return $ commentOrigin comment
          (replyWidget, enctype) <- generateFormPost $ commentForm userId userSl mediumId (Just commentId)
          defaultLayout $ do
            setTitle "Eidolon :: Reply to comment"
            $(widgetFile "commentReply")
        Nothing -> do
          setMessage "You need to be logged in to comment on media"
          redirect $ LoginR
    Nothing -> do
      setMessage "This comment does not Exist"
      redirect $ HomeR

postCommentReplyR :: CommentId -> Handler Html
postCommentReplyR commentId = do
  tempComment <- runDB $ get commentId
  case tempComment of
    Just comment -> do
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ Just $ getUserIdFromText tempUserId
          u <- runDB $ getJust $ fromJust userId
          userSl <- return $ Just $ userSlug u
          mediumId <- return $ commentOrigin comment
          ((res, commentWidget), enctype) <- runFormPost $ commentForm userId userSl mediumId (Just commentId)
          case res of
            FormSuccess temp -> do
              cId <- runDB $ insert temp
              --send mail to parent author
              parent <- runDB $ getJust $ fromJust $ commentParent temp
              parAuth <- runDB $ getJust $ fromJust $ commentAuthor parent
              link <- ($ MediumR (commentOrigin temp)) <$> getUrlRender
              sendMail (userEmail parAuth) ((fromJust $ commentAuthorSlug temp) `T.append` " replied to your comment")
                [shamlet|
                  <h1>Hello #{userSlug parAuth}
                  <p>#{fromJust $ commentAuthorSlug temp} replied to your comment:
                  #{commentContent temp}
                  <p>To see the comment thread follow
                    <a href=#{link}>
                      this link
                    .
                  |]
              --send mail to medium owner
              medium <- runDB $ getJust mediumId
              owner <- runDB $ getJust $ mediumOwner medium
              sendMail (userEmail owner) ((fromJust $ commentAuthorSlug temp) `T.append` " commented on your medium")
                [shamlet|
                  <h1>Hello #{userSlug owner}
                  <p>#{fromJust $ commentAuthorSlug temp} commented your medium with:
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
          redirect $ LoginR
    Nothing -> do
      setMessage "This comment does not exist!"
      redirect $ HomeR

getCommentDeleteR :: CommentId -> Handler Html
getCommentDeleteR commentId = do
  tempComment <- runDB $ get commentId
  case tempComment of
    Just comment -> do
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          presence <- return $ (Just userId) == (commentAuthor comment)
          case presence of
            True -> do
              defaultLayout $ do
                setTitle "Eidolon :: Delete comment"
                $(widgetFile "commentDelete")
            False -> do
              setMessage "You must be the author of this comment to delete it"
              redirect $ MediumR $ commentOrigin comment
        Nothing -> do
          setMessage "You must be logged in to delete comments"
          redirect $ LoginR
    Nothing -> do
      setMessage "This comment does not exist"
      redirect $ HomeR

postCommentDeleteR :: CommentId -> Handler Html
postCommentDeleteR commentId = do
  tempComment <- runDB $ get commentId
  case tempComment of
    Just comment -> do
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          presence <- return $ (Just userId) == (commentAuthor comment)
          case presence of
            True -> do
              confirm <- lookupPostParam "confirm"
              case confirm of
                Just "confirm" -> do
                  -- delete comment children
                  childEnts <- runDB $ selectList [CommentParent ==. (Just commentId)] []
                  mapM (\ent -> runDB $ delete $ entityKey ent) childEnts
                  -- delete comment itself
                  runDB $ delete commentId
                  -- outro
                  setMessage "Your comment has been deleted"
                  redirect $ MediumR $ commentOrigin comment
                _ -> do
                  setMessage "You must confirm the deletion"
                  redirect $ MediumR $ commentOrigin comment
            False -> do
              setMessage "You must be the author of this comment to delete it"
              redirect $ MediumR $ commentOrigin comment
        Nothing -> do
          setMessage "You must be logged in to delete comments"
          redirect $ LoginR
    Nothing -> do
      setMessage "This comment does not exist"
      redirect $ HomeR
