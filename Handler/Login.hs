module Handler.Login where

import Import hiding (returnJson)
import qualified Data.Text as T
import Crypto.HMAC
import Crypto.Hash.CryptoAPI (SHA1)
import Data.Text.Encoding (encodeUtf8)
import Data.Serialize (encode)
import Data.Maybe
import qualified Data.ByteString as B
import Data.Aeson.Types

data Credentials = Credentials
  { credentialsName   :: Text
  , credentialsPasswd :: Text
  }
  deriving Show

getLoginR :: Handler Html
getLoginR = do
--  (loginWidget, enctype) <- generateFormPost loginForm
  formLayout $ do
    setTitle "Eidolon :: Login"
    $(widgetFile "login")


postLoginR :: Handler RepJson
postLoginR = do
  mUserName <- lookupPostParam "username"
  mHexToken <- lookupPostParam "token"
  mHexResponse <- lookupPostParam "response"

  case (mUserName, mHexToken, mHexResponse) of
    (Just userName, Nothing, Nothing) -> do
      tempUser <- runDB $ selectFirst [UserName ==. userName] []
      case tempUser of
        Just (Entity userId user) -> do
          salt <- return $ userSalt user
          token <- liftIO makeRandomToken
          _ <- runDB $ insert $ Token (encodeUtf8 token) "login" (Just userId)
          returnJson ["salt" .= (toHex salt), "token" .= (toHex $ encodeUtf8 token)]
        Nothing ->
          returnJsonError ("No such user" :: T.Text)

    (Nothing, Just hexToken, Just hexResponse) -> do
      response <- do
        tempToken <- return $ fromHex' $ T.unpack hexToken
        savedToken <- runDB $ selectFirst [TokenKind ==. "login", TokenToken ==. tempToken] []
        case savedToken of
          Just (Entity tokenId token) -> do
            savedUserId <- return $ tokenUser token
            queriedUser <- runDB $ getJust (fromJust savedUserId)
            salted <- return $ userSalted queriedUser
            hexSalted <- return $ toHex salted
            expected <- return $ hmacSHA1 (tokenToken token) (encodeUtf8 hexSalted)
            case (fromHex' $ T.unpack hexResponse) == expected of
              True -> do
                -- Success!!
                runDB $ delete tokenId
                return $ Right savedUserId
              _    ->
                return $ Left ("Wrong password" :: T.Text)
          Nothing ->
            return $ Left "Invalid token"
      case response of
        Left msg ->
          returnJsonError msg
        Right userId -> do
          setSession "userId" $ extractKey (fromJust userId)
          welcomeLink <- ($ProfileR (fromJust userId)) <$> getUrlRender
          returnJson ["welcome" .= welcomeLink]

    _ ->
      returnJsonError ("Protocol error" :: T.Text)


loginForm :: Form Credentials
loginForm = renderDivs $ Credentials
  <$> areq textField "Username" Nothing
  <*> areq passwordField "Password" Nothing

getLogoutR :: Handler Html
getLogoutR = do
  deleteSession "userId"
  setMessage "Succesfully logged out"
  redirect $ HomeR

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
