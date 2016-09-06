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

{-# LANGUAGE TupleSections, OverloadedStrings #-} 
module Handler.Signup where

import Import as I
import Data.Text as T
import Data.Text.Encoding
import Data.Maybe

getSignupR :: Handler Html
getSignupR = do
  master <- getYesod
  let block = appSignupBlocked $ appSettings master
  case block of
    False -> do
      formLayout $ do
        setTitle "Eidolon :: Signup"
        $(widgetFile "signup")
    True -> do
      setMessage "User signup has been disabled"
      redirect $ HomeR

postSignupR :: Handler Html
postSignupR = do
  master <- getYesod
  let block = appSignupBlocked $ appSettings master
  case block of
    False -> do
      mUserName <- lookupPostParam "username"
      newUserName <- case validateLen (fromJust mUserName) of
        True -> return $ fromJust $ mUserName
        False -> do
          setMessage "Invalid username"
          redirect SignupR
      mEmail <- lookupPostParam "email"
      mTos1 <- lookupPostParam "tos-1"
      mTos2 <- lookupPostParam "tos-2"
      case (mTos1, mTos2) of
        (Just "tos-1", Just "tos-2") ->
          return ()
        _ -> do
          setMessage "You need to agree to our terms."
          redirect SignupR
      -- create user
      namesakes <- runDB $ selectList [UserName ==. newUserName] []
      case namesakes of
        [] -> do
          salt <- liftIO generateSalt
          let newUser = User newUserName newUserName (fromJust mEmail) salt "" [] False
          activatorText <- liftIO generateString
          _ <- runDB $ insert $ Activator activatorText newUser
          _ <- runDB $ insert $ Token (encodeUtf8 activatorText) "activate" Nothing
          activateLink <- ($ ActivateR activatorText) <$> getUrlRender
          sendMail (userEmail newUser) "Please activate your account!" $
            [shamlet|
              <h1>Hello #{userSlug newUser} and Welcome to Eidolon!
              To complete your signup please activate your account by visiting the following link:
              <a href="#{activateLink}">#{activateLink}
            |]
          setMessage "User activation pending"
          redirect HomeR
        _ -> do
          setMessage "This user already exists"
          redirect SignupR
    True -> do
      setMessage "User signup is disabled"
      redirect HomeR

validateLen :: Text -> Bool
validateLen a =
  (T.length a) > 2
