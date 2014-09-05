module Handler.ProfileDelete where

import Import
import qualified Data.Text as T
import Data.Maybe
import System.Directory
import System.FilePath

getProfileDeleteR :: UserId -> Handler Html
getProfileDeleteR userId = do
  tempUser <- runDB $ get userId
  case tempUser of
    Just user -> do
      username <- return $ userName user
      msu <- lookupSession "userId"
      case msu of
        Just tempLoginId -> do
          loginId <- return $ getUserIdFromText tempLoginId
          case loginId == userId of
            True -> do
              defaultLayout $ do
                $(widgetFile "profileDelete")
            False -> do
              setMessage "You can only delete your own profile"
              redirect $ UserR username
        Nothing -> do
          setMessage "You must be logged in to delete profiles"
          redirect $ LoginR
    Nothing -> do
      setMessage "This user does not exist"
      redirect $ HomeR

postProfileDeleteR :: UserId -> Handler Html
postProfileDeleteR userId = do
  tempUser <- runDB $ get userId
  case tempUser of
    Just user -> do
      username <- return $ userName user
      msu <- lookupSession "userId"
      case msu of
        Just tempLoginId -> do
          loginId <- return $ getUserIdFromText tempLoginId
          case loginId == userId of
            True -> do
              confirm <- lookupPostParam "confirm"
              case confirm of
                Just "confirm" -> do
                  albumList <- return $ userAlbums user
                  mapM (\albumId -> do
                    album <- runDB $ getJust albumId
                    mediaList <- return $ albumContent album
                    mapM (\med -> runDB $ delete med) mediaList
                    runDB $ delete albumId
                    ) albumList
                  runDB $ delete userId
                  liftIO $ removeDirectoryRecursive $ "static" </> "data" </> (T.unpack $ extractKey userId)
                  deleteSession "userId"
                  setMessage "User deleted successfully"
                  redirect $ HomeR
                _ -> do
                  setMessage "You must confirm the deletion"
                  redirect $ ProfileSettingsR userId
            False -> do
              setMessage "You can only delete your own profile"
              redirect $ UserR username
        Nothing -> do
          setMessage "You must be logged in to delete profiles"
          redirect $ LoginR
    Nothing -> do
      setMessage "This user does not exist"
      redirect $ HomeR
