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

module Handler.Login where

import Import hiding (returnJson)
import qualified Data.Text as T

-- old hmac
import Crypto.HMAC as Old
import Crypto.Hash.CryptoAPI (SHA1)

-- new hmac
import Crypto.MAC.HMAC as New
import Crypto.Hash.Algorithms (Keccak_512)

import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Serialize (encode)
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Aeson.Types

-- getLoginR :: Handler Html
-- getLoginR = do
--   master <- getYesod
--   let addWarn = "http://" `T.isPrefixOf` appRoot (appSettings master)
--   (loginRawWidget, _) <- generateFormPost $
--     renderBootstrap3 BootstrapBasicForm loginForm
--   defaultLayout $ do
--     setTitle "Eidolon :: Login"
--     $(widgetFile "login")


-- postLoginR :: Handler RepJson
-- postLoginR = do
--   mUserName <- lookupPostParam "username"
--   mHexToken <- lookupPostParam "token"
--   mHexResponse <- lookupPostParam "response"
--   case (mUserName, mHexToken, mHexResponse) of
--     (Just userName, Nothing, Nothing) -> do
--       tempUser <- runDB $ getBy $ UniqueUser userName
--       case tempUser of
--         Just (Entity userId user) -> do
--           let salt = userSalt user
--           token <- liftIO makeRandomToken
--           runDB $ insert_ $ Token (encodeUtf8 token) "login" userName
--           returnJson ["salt" .= toHex salt, "token" .= toHex (encodeUtf8 token)]
--         Nothing ->
--           returnJsonError ("No such user" :: T.Text)
--     (Nothing, Just hexToken, Just hexResponse) -> do
--       response <- do
--         let tempToken = fromHex' $ T.unpack hexToken
--         savedToken <- runDB $ selectFirst [TokenKind ==. "login", TokenToken ==. tempToken] []
--         case savedToken of
--           Just (Entity tokenId token) -> do
--             let savedUserName = tokenUsername token
--             mqueriedUser <- runDB $ getBy $ UniqueUser savedUserName
--             let queriedUser = entityVal $ fromJust mqueriedUser
--                 salted = userSalted queriedUser
--                 hexSalted = toHex salted
--                 expected = hmacKeccak (encodeUtf8 $ toHex $ tokenToken token) (encodeUtf8 hexSalted)
--             if encodeUtf8 hexResponse == expected
--               then do
--                 -- Success!!
--                 runDB $ delete tokenId
--                 return $ Right $ (entityKey $ fromJust mqueriedUser)
--               else
--                 return $ Left ("Wrong password" :: T.Text)
--           Nothing ->
--             return $ Left "Invalid token"
--       case response of
--         Left msg ->
--           returnJsonError msg
--         Right userId -> do
--           setSession "userId" $ extractKey userId
--           setMessage "Succesfully logged in"
--           welcomeLink <- ($ProfileR userId) <$> getUrlRender
--           returnJson ["welcome" .= welcomeLink]
--     _ ->
--       returnJsonError ("Protocol error" :: T.Text)

getLoginRawR :: Handler Html
getLoginRawR = do
  master <- getYesod
  let addWarn = "http://" `T.isPrefixOf` appRoot (appSettings master)
  (loginRawWidget, _) <- generateFormPost $
    renderBootstrap3 BootstrapBasicForm loginForm
  defaultLayout $ do
    setTitle "Eidolon :: Login"
    $(widgetFile "login")

postLoginRawR :: Handler Html
postLoginRawR = do
  ((res, _), _) <- runFormPost $
    renderBootstrap3 BootstrapBasicForm loginForm
  case res of
    FormSuccess cred -> do
      muser <- runDB $ getBy $ UniqueUser $ credentialsName cred
      case muser of
        Just (Entity uId user) -> do
          let testSalted = BC.unpack $ hmacKeccak (userSalt user) (encodeUtf8 $ credentialsPasswd cred)
          if fromHex' testSalted == userSalted user
          then do
            setCreds False $ Creds "raw" (userName user) []
            setMessage "Successfully logged in"
            redirect $ ProfileR uId
          else do
            setMessage "Wrong password"
            redirect $ AuthR LoginR
        Nothing -> do
          setMessage "No such user"
          redirect $ AuthR LoginR
    _ -> do
      setMessage "Login error"
      redirect $ AuthR LoginR

data Credentials = Credentials
  { credentialsName   :: Text
  , credentialsPasswd :: Text
  }
  deriving Show

loginForm :: AForm Handler Credentials
loginForm = Credentials
  <$> areq textField (bfs ("Username" :: T.Text)) Nothing
  <*> areq passwordField (bfs ("Password" :: T.Text)) Nothing
  <*  bootstrapSubmit ("Login" :: BootstrapSubmit T.Text)

getLogoutR :: Handler Html
getLogoutR = do
  deleteSession "userId"
  setMessage "Succesfully logged out"
  redirect HomeR

returnJson :: Monad m => [Pair] -> m RepJson
returnJson = return . repJson . object

returnJsonError :: (ToJSON a, Monad m) => a -> m RepJson
returnJsonError = returnJson . (:[]) . ("error" .=)

hmacSHA1 :: B.ByteString -> B.ByteString -> B.ByteString
hmacSHA1 keyData msgData =
  let key = MacKey keyData
      sha1 :: SHA1
      sha1 = hmac' key msgData
  in encode sha1

hmacKeccak :: B.ByteString -> B.ByteString -> B.ByteString
hmacKeccak key msg = BC.pack $ show $ hmacGetDigest (New.hmac key msg :: HMAC Keccak_512)
