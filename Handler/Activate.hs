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
module Handler.Activate where

import Import as I hiding (returnJson)
import Data.Text
import Data.Text.Encoding
import Data.Maybe
import System.Directory
import System.FilePath

getActivateR :: Text -> Handler Html
getActivateR token = do
  t <- runDB $ selectFirst [ActivatorToken ==. token] []
  case t of
    Nothing -> do
      mToken <- runDB $ selectFirst [TokenToken ==. encodeUtf8 token, TokenKind ==. "activate"] []
      case mToken of
        Just (Entity _ uToken) -> do
          user <- runDB $ getJust (fromJust $ tokenUser uToken)
          let hexSalt = toHex $ userSalt user
          formLayout $ do
            setTitle "Activate your account"
            $(widgetFile "activate")
        _ -> do
          setMessage "Invalid token!"
          redirect HomeR
    Just (Entity _ activator) -> do
      let uSalt = userSalt $ activatorUser activator
      mToken <- runDB $ selectFirst [TokenToken ==. encodeUtf8 token, TokenKind ==. "activate"] []
      case mToken of
        Just (Entity _ _) -> do
          let hexSalt = toHex uSalt
          formLayout $
            $(widgetFile "activate")
        _ -> do
          setMessage "Invalid token!"
          redirect HomeR

postActivateR :: Text -> Handler RepJson
postActivateR token = do
  msalted <- fromJust <$> lookupPostParam "salted"
  let salted = fromHex' $ unpack msalted
  mToken <- runDB $ selectFirst [TokenToken ==. encodeUtf8 token, TokenKind ==. "activate"] []
  case mToken of
    Just (Entity uTokenId uToken) ->
      if
        isNothing (tokenUser uToken)
        then do
          newUser <- runDB $ selectFirst [ActivatorToken ==. token] []
          case newUser of
            Just (Entity aId activ) -> do
              namesakes <- runDB $ selectList [UserName ==. userName (activatorUser activ)] []
              if
                I.null namesakes
                then do
                  -- putting user in active state
                  uId <- runDB $ insert $ activatorUser activ
                  runDB $ update uId [UserSalted =. salted]
                  -- create user directory
                  liftIO $ createDirectoryIfMissing True $
                    "static" </> "data" </> unpack (extractKey uId)
                  -- input user to elasticsearch
                  liftIO $ putIndexES (ESUser uId $ activatorUser activ)
                  -- cleanup
                  runDB $ delete aId
                  runDB $ delete uTokenId
                  -- login and redirect
                  setSession "userId" (extractKey uId)
                  welcomeLink <- ($ ProfileR uId) <$> getUrlRender
                  returnJson ["welcome" .= welcomeLink]
                else do
                  -- cleanup
                  runDB $ delete aId
                  runDB $ delete uTokenId
                  returnJsonError "Somebody already activated your username. Your token has been deleted"
            Nothing ->
              returnJsonError "Invalid token"
        else do
          runDB $ update (fromJust $ tokenUser uToken) [UserSalted =. salted]
          -- cleanup
          runDB $ delete uTokenId
          setSession "userId" (extractKey $ fromJust $ tokenUser uToken)
          welcomeLink <- ($ ProfileR (fromJust $ tokenUser uToken)) <$> getUrlRender
          returnJson ["welcome" .= welcomeLink]
    _ ->
      returnJsonError "Invalid activation token!"

returnJson :: (Monad m, ToJSON a, a ~ Value) =>
              [(Text, a)] -> m RepJson
returnJson = return . repJson . object

returnJsonError :: Text -> Handler RepJson
returnJsonError = returnJson . (:[]) . ("error" .=) 
