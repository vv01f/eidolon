module Handler.NewAlbum where

import Import
import Data.Text
import System.Directory
import System.FilePath

getNewAlbumR :: Handler Html
getNewAlbumR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- lift $ pure $ getUserIdFromText tempUserId
      (albumWidget, enctype) <- generateFormPost (albumForm userId)
      defaultLayout $ do
        $(widgetFile "newAlbum")
    Nothing -> do
      setMessage "You need to be logged in"
      redirect $ LoginR

postNewAlbumR :: Handler Html
postNewAlbumR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- lift $ pure $ getUserIdFromText tempUserId
      ((result, albumWidget), enctype) <- runFormPost (albumForm userId)
      case result of
        FormSuccess album -> do
          albumId <- runDB $ insert album
          liftIO $ createDirectory $ "static" </> "data" </> (unpack $ extractKey userId) </> (unpack $ extractKey albumId)
          setMessage $ "Album successfully created"
          redirect $ ProfileR userId

albumForm :: UserId -> Form Album
albumForm userId = renderDivs $ Album
  <$> areq textField "Title" Nothing
  <*> pure userId
  <*> pure []
  <*> pure Nothing
