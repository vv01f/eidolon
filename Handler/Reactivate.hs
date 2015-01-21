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

module Handler.Reactivate where

import Import
import Control.Monad
import Data.Text.Encoding

getReactivateR :: Handler Html
getReactivateR = do
  (reactivateWidget, enctype) <- generateFormPost reactivateForm
  defaultLayout $ do
    setTitle "Eidolon :: Reactivate account"
    $(widgetFile "reactivate")

postReactivateR :: Handler Html
postReactivateR = do
  ((result, _), _) <- runFormPost reactivateForm
  case result of
    FormSuccess temp -> do
      users <- runDB $ selectList [UserEmail ==. temp] []
      case null users of
        True -> do
          userTokens <- foldM (\userTokens (Entity userId user) -> do
            token <- liftIO $ generateString
            _ <- runDB $ insert $ Token (encodeUtf8 token) "activate" (Just userId)
            return $ (user, token) : userTokens
            ) [] users
          _ <- foldM (\sent (user, token) ->
            case sent of
              False ->
                return False
              True -> do
                activateLink <- ($ ActivateR token) <$> getUrlRender
                sendMail (userEmail user) "Reset your password" $
                  [shamlet|
                    <h1>Welcome again to Eidolon #{userName user}
                    To reset your password visit the following link:
                    <a href="#{activateLink}">#{activateLink}

                    See you soon!
                  |]
                return True
            ) True userTokens
          setMessage "Your new password activation will arrive in your e-mail"
          redirect $ HomeR
        False -> do
          setMessage "No user mith this Email"
          redirect $ LoginR
    _ -> do
      setMessage "There is something wrong with your email"
      redirect $ ReactivateR

reactivateForm :: Form Text
reactivateForm = renderDivs $ (\a -> a)
  <$> areq emailField "Email"  Nothing
