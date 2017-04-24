--  eidolon -- A simple gallery in Haskell and Yesod
--  Copyright (C) 2015  Amedeo Molnár
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Affero General Public License as published
--  by the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Affero General Public License for more details.
--
--  You should have received a copy of the GNU Affero General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Handler.ProfileSettings where

import Import
import qualified Data.Text as T
import Handler.Commons

getProfileSettingsR :: UserId -> Handler Html
getProfileSettingsR userId = do
  checkRes <- profileCheck userId
  case checkRes of
    Right user -> do
      (profileSettingsWidget, enctype) <- generateFormPost $
        renderBootstrap3 BootstrapBasicForm $ profileSettingsForm user
      defaultLayout $ do
        setTitle "Eidolon :: Profile settings"
        $(widgetFile "profileSettings")
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route

postProfileSettingsR :: UserId -> Handler Html
postProfileSettingsR userId = do
  checkRes <- profileCheck userId
  case checkRes of
    Right user -> do
      ((result, _), _) <- runFormPost $
        renderBootstrap3 BootstrapBasicForm$ profileSettingsForm user
      case result of
        FormSuccess temp -> do
         runDB $ update userId [
             UserName =. userName temp
           , UserSlug =. userSlug temp
           , UserEmail =. userEmail temp
           , UserDefaultLicence =. (userDefaultLicence temp)
           ]
         setMessage "Profile settings changed successfully"
         redirect $ UserR $ userName user
        _ -> do
          setMessage "There was an error changing your settings"
          redirect $ ProfileSettingsR userId
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route


profileSettingsForm :: User -> AForm Handler User
profileSettingsForm user = User
  <$> areq textField (bfs ("Username" :: T.Text)) (Just $ userName user)
  <*> areq textField (bfs ("Userslug" :: T.Text)) (Just $ userSlug user)
  <*> areq emailField (bfs ("Email" :: T.Text)) (Just $ userEmail user)
  <*> pure (userSalt user)
  <*> pure (userSalted user)
  <*> pure (userAlbums user)
  <*> pure (userAdmin user)
  <*> areq (selectField licences) (bfs ("Default licence for media" :: T.Text))
    (Just $ userDefaultLicence user)
  <*> pure (userActive user)
  <*  bootstrapSubmit ("Change settings" :: BootstrapSubmit Text)
  where
    licences = optionsPairs $ map (\a -> (T.pack (show (toEnum a :: Licence)), a)) [-2..6]
