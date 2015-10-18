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
import Handler.Commons

getProfileSettingsR :: UserId -> Handler Html
getProfileSettingsR userId = do
  checkRes <- profileCheck userId
  case checkRes of
    Right user -> do
      (profileSettingsWidget, enctype) <- generateFormPost $ profileSettingsForm user
      formLayout $ do
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
      ((result, _), _) <- runFormPost $ profileSettingsForm user
      case result of
        FormSuccess temp -> do
         runDB $ update userId [
             UserName =. userName temp
           , UserSlug =. userSlug temp
           , UserEmail =. userEmail temp
           ]
         putIndexES (ESUser userId temp)
         setMessage "Profile settings changed successfully"
         redirect $ UserR $ userName user
        _ -> do
          setMessage "There was an error changing your settings"
          redirect $ ProfileSettingsR userId
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route


profileSettingsForm :: User -> Form User
profileSettingsForm user = renderDivs $ User
  <$> areq textField "Username" (Just $ userName user)
  <*> areq textField "Userslug" (Just $ userSlug user)
  <*> areq emailField "Email" (Just $ userEmail user)
  <*> pure (userSalt user)
  <*> pure (userSalted user)
  <*> pure (userAlbums user)
  <*> pure (userAdmin user)
