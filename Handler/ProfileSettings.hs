module Handler.ProfileSettings where

import Import

getProfileSettingsR :: UserId -> Handler Html
getProfileSettingsR userId = do
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
              (profileSettingsWidget, enctype) <- generateFormPost $ profileSettingsForm user
              defaultLayout $ do
                setTitle "Eidolon :: Profile settings"
                $(widgetFile "profileSettings")
            False -> do
              setMessage "You can only change your own profile settings"
              redirect $ UserR username
        Nothing -> do
          setMessage "You need to be logged in to change settings"
          redirect $ LoginR
    Nothing -> do
      setMessage "This user does not exist"
      redirect $ HomeR

postProfileSettingsR :: UserId -> Handler Html
postProfileSettingsR userId = do
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
              ((result, profileSettingsWidget), enctype) <- runFormPost $ profileSettingsForm user
              case result of
                FormSuccess temp -> do
                  runDB $ update userId [
                      UserName =. (userName temp)
                    , UserSlug =. (userSlug temp)
                    , UserEmail =. (userEmail temp)
                    ]
                  setMessage "Profile settings changed successfully"
                  redirect $ UserR username
                _ -> do
                  setMessage "There was an error changing your settings"
                  redirect $ ProfileSettingsR userId
            False -> do
              setMessage "You can only change your own profile settings"
              redirect $ UserR username
        Nothing -> do
          setMessage "You need to be logged in to change settings"
          redirect $ LoginR
    Nothing -> do
      setMessage "This user does not exist"
      redirect $ HomeR


profileSettingsForm :: User -> Form User
profileSettingsForm user = renderDivs $ User
  <$> areq textField "Username" (Just $ userName user)
  <*> areq textField "Userslug" (Just $ userSlug user)
  <*> areq emailField "Email" (Just $ userEmail user)
  <*> pure (userSalt user)
  <*> pure (userSalted user)
  <*> pure (userAlbums user)
  <*> pure (userAdmin user)
