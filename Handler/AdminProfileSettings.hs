module Handler.AdminProfileSettings where

import Import
import qualified Data.Text as T
import System.Directory
import System.FilePath

getAdminProfilesR :: Handler Html
getAdminProfilesR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True -> do
          profiles <- runDB $ selectList [] [Desc UserName]
          defaultLayout $ do
            $(widgetFile "adminProfiles")
        False -> do
          setMessage "You are no admin"
          redirect $ HomeR
    Nothing -> do
      setMessage "You must be logged in"
      redirect $ LoginR

getAdminUserAlbumsR :: UserId -> Handler Html
getAdminUserAlbumsR ownerId = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True -> do
          tempOwner <- runDB $ get ownerId
          case tempOwner of
            Just owner -> do
              albums <- runDB $ selectList [AlbumOwner ==. ownerId] [Desc AlbumTitle]
              defaultLayout $ do
                $(widgetFile "adminUserAlbums")
            Nothing -> do
              setMessage "This user does not exist"
              redirect $ AdminR
        False -> do
          setMessage "You are no admin"
          redirect $ HomeR
    Nothing -> do
      setMessage "You must be logged in"
      redirect $ LoginR

getAdminProfileSettingsR :: UserId -> Handler Html
getAdminProfileSettingsR ownerId = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True -> do
          tempOwner <- runDB $ get ownerId
          case tempOwner of
            Just owner -> do
              (adminProfileSetWidget, enctype) <- generateFormPost $ adminProfileForm owner
              defaultLayout $ do
                $(widgetFile "adminProfileSettings")
            Nothing -> do
              setMessage "This user does not exist"
              redirect $ AdminR
        False -> do
          setMessage "You are not an admin"
          redirect $ HomeR
    Nothing -> do
      setMessage "You are not logged in"
      redirect $ LoginR
      

postAdminProfileSettingsR :: UserId -> Handler Html
postAdminProfileSettingsR ownerId = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True -> do
          tempOwner <- runDB $ get ownerId
          case tempOwner of
            Just owner -> do
              ((result, adminProfileSetWidget), enctype) <- runFormPost $ adminProfileForm owner
              case result of
                FormSuccess temp -> do
                  runDB $ update ownerId 
                    [ UserName =. (userName temp)
                    , UserSlug =. (userSlug temp)
                    , UserEmail =. (userEmail temp)
                    , UserAdmin =. (userAdmin temp)
                    ]
                  setMessage "User data updated successfully"
                  redirect $ AdminR
                _ -> do
                  setMessage "There was an error"
                  redirect $ AdminProfileSettingsR ownerId
            Nothing -> do
              setMessage "This user does not exist"
              redirect $ AdminR
        False -> do
          setMessage "You are not an admin"
          redirect $ HomeR
    Nothing -> do
      setMessage "You are not logged in"
      redirect $ LoginR


adminProfileForm :: User -> Form User
adminProfileForm owner = renderDivs $ User
  <$> areq textField "Username" (Just $ userName owner)
  <*> areq textField "Userslug" (Just $ userSlug owner)
  <*> areq emailField "Email" (Just $ userEmail owner)
  <*> pure (userSalt owner)
  <*> pure (userSalted owner)
  <*> pure (userAlbums owner)
  <*> areq boolField "Admin" (Just $ userAdmin owner)

getAdminProfileDeleteR :: UserId -> Handler Html
getAdminProfileDeleteR ownerId = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True -> do
          tempOwner <- runDB $ get ownerId
          case tempOwner of
            Just owner -> do
              albumList <- return $ userAlbums owner
              mapM (\albumId -> do
                album <- runDB $ getJust albumId
                mediaList <- return $ albumContent album
                mapM (\med -> runDB $ delete med) mediaList
                runDB $ delete albumId
                ) albumList
              runDB $ delete ownerId
              liftIO $ removeDirectoryRecursive $ "static" </> "data" </> (T.unpack $ extractKey ownerId)
              setMessage "User successfully deleted"
              redirect $ AdminR
            Nothing -> do
              setMessage "This user does not exist"
              redirect $ AdminR
        False -> do
          setMessage "You are no admin"
          redirect $ HomeR
    Nothing -> do
      setMessage "You must be logged in"
      redirect $ LoginR
