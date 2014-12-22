module Handler.AdminComments where

import Import
import Data.Maybe
import Data.Time
import System.Locale

getAdminCommentR :: Handler Html
getAdminCommentR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True -> do
          media <- runDB $ selectList [] [Desc MediumTime]
          comments <- runDB $ selectList [CommentParent ==. Nothing] [Desc CommentTime]
          replies <- runDB $ selectList [CommentParent !=. Nothing] [Desc CommentTime]
          defaultLayout $ do
            setTitle "Administration: Comments"
            $(widgetFile "adminComments")
        False -> do
          setMessage "You have no admin rights"
          redirect $ HomeR
    Nothing -> do
      setMessage "You are not logged in"
      redirect $ LoginR

getAdminCommentDeleteR :: CommentId -> Handler Html
getAdminCommentDeleteR commentId = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True -> do
          tempComment <- runDB $ get commentId
          case tempComment of
            Just comment -> do
              -- delete comment children
              children <- runDB $ selectList [CommentParent ==. (Just commentId)] []
              mapM (\ent -> runDB $ delete $ entityKey ent) children
              -- delete comment itself
              runDB $ delete commentId
              setMessage "Comment deleted succesfully"
              redirect $ AdminR
            Nothing -> do
              setMessage "This comment does not exist"
              redirect $ AdminR
        False -> do
          setMessage "You are no admin"
          redirect $ HomeR
    Nothing -> do
      setMessage "You must be logged in"
      redirect $ LoginR
