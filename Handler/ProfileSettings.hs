module Handler.ProfileSettings where

import Import
import Helper

getProfileSettingsR :: UserId -> Handler Html
getProfileSettingsR userId = do
  checkRes <- profileCheck userId
  case checkRes of
    Right user -> do
      (profileSettingsWidget, enctype) <- generateFormPost $ profileSettingsForm user
      defaultLayout $ do
        setTitle "Eidolon :: Profile settings"
        $(widgetFile "profileSettings")
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect $ route

postProfileSettingsR :: UserId -> Handler Html
postProfileSettingsR userId = do
  checkRes <- profileCheck userId
  case checkRes of
    Right user -> do
      ((result, profileSettingsWidget), enctype) <- runFormPost $ profileSettingsForm user
      case result of
        FormSuccess temp -> do
         runDB $ update userId [
             UserName =. (userName temp)
           , UserSlug =. (userSlug temp)
           , UserEmail =. (userEmail temp)
           ]
         setMessage "Profile settings changed successfully"
         redirect $ UserR $ userName user
        _ -> do
          setMessage "There was an error changing your settings"
          redirect $ ProfileSettingsR userId
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect $ route


profileSettingsForm :: User -> Form User
profileSettingsForm user = renderDivs $ User
  <$> areq textField "Username" (Just $ userName user)
  <*> areq textField "Userslug" (Just $ userSlug user)
  <*> areq emailField "Email" (Just $ userEmail user)
  <*> pure (userSalt user)
  <*> pure (userSalted user)
  <*> pure (userAlbums user)
  <*> pure (userAdmin user)
