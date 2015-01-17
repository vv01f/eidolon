module Handler.AdminComments where

import Import
import Handler.Commons
import Data.Maybe
import Data.Time
import System.Locale

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
      redirect $ route

getAdminCommentDeleteR :: CommentId -> Handler Html
getAdminCommentDeleteR commentId = do
  adminCheck <- loginIsAdmin
  case adminCheck of
    Right _ -> do
      tempComment <- runDB $ get commentId
      case tempComment of
        Just _ -> do
          -- delete comment children
          children <- runDB $ selectList [CommentParent ==. (Just commentId)] []
          _ <- mapM (\ent -> runDB $ delete $ entityKey ent) children
          -- delete comment itself
          runDB $ delete commentId
          setMessage "Comment deleted succesfully"
          redirect $ AdminR
        Nothing -> do
          setMessage "This comment does not exist"
          redirect $ AdminR
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect $ route
