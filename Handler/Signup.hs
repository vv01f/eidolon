{-# LANGUAGE TupleSections, OverloadedStrings #-} 
module Handler.Signup where

import Import as I
import System.Random
import Data.Text as T
import Data.Text.Encoding
import Data.ByteString as B
import Data.Maybe
import Network.Mail.Mime
import Text.Blaze.Html.Renderer.Utf8

getSignupR :: Handler Html
getSignupR = do
--  (signupWidget, enctype) <- generateFormPost signupForm
  defaultLayout $ do
    $(widgetFile "signup")

postSignupR :: Handler Html
postSignupR = do
  mUserName <- lookupPostParam "username"
  newUserName <- case validateLen (fromJust mUserName) of
    True -> return $ fromJust $ mUserName
    False -> do
      setMessage "Invalid username"
      redirect $ SignupR
  mEmail <- lookupPostParam "email"
  mTos1 <- lookupPostParam "tos-1"
  mTos2 <- lookupPostParam "tos-2"
  case (mTos1, mTos2) of
    (Just "tos-1", Just "tos-2") ->
      return ()
    _ -> do
      setMessage "You need to agree to our terms."
      redirect $ SignupR

  -- create user
  namesakes <- runDB $ selectList [UserName ==. newUserName] []
  case namesakes of
    [] -> do
      salt <- liftIO generateSalt
      newUser <- return $ User newUserName newUserName (fromJust mEmail) salt "" []
      activatorText <- liftIO generateString
      aId <- runDB $ insert $ Activator activatorText newUser
      tId <- runDB $ insert $ Token (encodeUtf8 activatorText) "activate" Nothing
      activateLink <- ($ ActivateR activatorText) <$> getUrlRender
      sendMail (userEmail newUser) "Please activate your account!" $
        [shamlet|
          <h1> Welcome to Eidolon!
          To complete your sgnup please activate your account by visiting the following link:
          <a href="#{activateLink}">#{activateLink}
        |]
      setMessage "User pending activation"
      redirect $ HomeR
    _ -> do
      setMessage "This user already exists"
      redirect $ SignupR

--signupForm :: Form User
--signupForm = renderDivs $ User
--  <$> areq textField "Username" Nothing
--  <*> areq emailField "Email" Nothing
--  <*> areq passwordField "Password" Nothing
--  <*> pure ("" :: ByteString)
--  <*> pure []

validateLen :: Text -> Bool
validateLen a =
  (T.length a) > 3

generateString :: IO T.Text
generateString = (toHex . B.pack . I.take 16 . randoms) <$> newStdGen

sendMail :: MonadIO m => Text -> Text -> Html -> m ()
sendMail toEmail subject body =
  liftIO $ renderSendMail
    Mail
      { mailFrom = Address Nothing "noreply" -- TODO: set sender Address
      , mailTo = [Address Nothing toEmail]
      , mailCc = []
      , mailBcc = []
      , mailHeaders = [("Subject", subject)]
      , mailParts = [[Part
        { partType = "text/html; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partHeaders = []
        , partContent = renderHtml body
        }]]
      }
