module Handler.Commons where

import Import
import Yesod
import Data.String

loginIsAdmin :: IsString t => Handler (Either (t, Route App)  ())
loginIsAdmin = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True ->
          return $ Right ()
        False ->
          return $ Left ("You have no admin rights", HomeR)
    Nothing ->
      return $ Left ("You are not logged in", LoginR)

profileCheck :: IsString t => UserId -> Handler (Either (t, Route App) User)
profileCheck userId = do
  tempUser <- runDB $ get userId
  case tempUser of
    Just user -> do
      msu <- lookupSession "userId"
      case msu of
        Just tempLoginId -> do
          loginId <- return $ getUserIdFromText tempLoginId
          case loginId == userId of
            True ->
              return $ Right user
            False ->
              return $ Left ("You can only change your own profile settings", UserR $ userName user)
        Nothing ->
          return $ Left ("You nedd to be logged in to change settings", LoginR)
    Nothing ->
      return $ Left ("This user does not exist", HomeR)

mediumCheck :: IsString t => MediumId -> Handler (Either (t, Route App) Medium)
mediumCheck mediumId = do
  tempMedium <- runDB $ get mediumId
  case tempMedium of
    Just medium -> do
      ownerId <- return $ mediumOwner medium
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          album <- runDB $ getJust $ mediumAlbum medium
          presence <- return (userId == ownerId)
          albumOwnerPresence <- return (userId == (albumOwner album))
          case presence || albumOwnerPresence of
            True ->
              return $ Right medium
            False ->
              return $ Left ("You must own this medium to change its settings", MediumR mediumId)
        Nothing ->
          return $ Left ("You must be logged in to change settings", LoginR)
    Nothing ->
      return $ Left ("This medium does not exist", HomeR)
