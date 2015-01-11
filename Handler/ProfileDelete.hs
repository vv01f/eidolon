module Handler.ProfileDelete where

import Import
import Handler.Commons
import qualified Data.Text as T
import qualified Data.List as L
import System.Directory
import System.FilePath

getProfileDeleteR :: UserId -> Handler Html
getProfileDeleteR userId = do
  checkRes <- profileCheck userId
  case checkRes of
    Right user -> do
      formLayout $ do
        setTitle "Eidolon :: Delete user profile"
        $(widgetFile "profileDelete")
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect $ route

postProfileDeleteR :: UserId -> Handler Html
postProfileDeleteR userId = do
  checkRes <- profileCheck userId
  case checkRes of
    Right user -> do
      confirm <- lookupPostParam "confirm"
      case confirm of
        Just "confirm" -> do
          albumList <- return $ userAlbums user
          _ <- mapM (\albumId -> do
            album <- runDB $ getJust albumId
            case (albumOwner album) == userId of
              True -> do
                mediaList <- return $ albumContent album
                _ <- mapM (\med -> do
                  commEnts <- runDB $ selectList [CommentOrigin ==. med] []
                  _ <- mapM (\ent -> runDB $ delete $ entityKey ent) commEnts
                  medium <- runDB $ getJust med
                  liftIO $ removeFile (normalise $ L.tail $ mediumPath medium)
                  liftIO $ removeFile (normalise $ L.tail $ mediumThumb medium)
                  runDB $ delete med
                  ) mediaList
                runDB $ delete albumId
              False -> return ()
            ) albumList
          runDB $ delete userId
          liftIO $ removeDirectoryRecursive $ "static" </> "data" </> (T.unpack $ extractKey userId)
          deleteSession "userId"
          setMessage "User deleted successfully"
          redirect $ HomeR
        _ -> do
          setMessage "You must confirm the deletion"
          redirect $ ProfileSettingsR userId
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect $ route
