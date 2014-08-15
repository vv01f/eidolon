{-# LANGUAGE TupleSections, OverloadedStrings #-} 
module Handler.Signup where

import Import as I
import System.Random
import Data.Text as T
import Network.Mail.Mime
import Text.Blaze.Html.Renderer.Utf8

getSignupR :: Handler Html
getSignupR = do
  (signupWidget, enctype) <- generateFormPost signupForm
  defaultLayout $ do
    $(widgetFile "signup")

postSignupR :: Handler Html
postSignupR = do
  ((result, signupWidget), enctype) <- runFormPost signupForm
  case result of
    FormSuccess user -> do
      namesakes <- runDB $ selectList [UserName ==. (userName user)] []
      case (validateLen (userName user)) && ((I.length namesakes) == 0) of
        True -> do
          tokenString <- liftIO generateString
          uId <- runDB $ insert $ Token tokenString user
          activateLink <- ($ ActivateR tokenString) <$> getUrlRender
          sendMail (userEmail user) "Please activate your account!" $
            [shamlet|
              <h1> Welcome to Eidolon!
              To complete your sgnup please activate your account by visiting the following link
              <a href="#{activateLink}">#{activateLink}
            |]
          setMessage "User created"
          redirect $ HomeR
        False -> do
          setMessage "Username error"
          redirect $ SignupR
    _ -> do
      setMessage "Please try again"
      redirect $ SignupR

signupForm :: Form User
signupForm = renderDivs $ User
  <$> areq textField "Username" Nothing
  <*> areq emailField "Email" Nothing
  <*> areq passwordField "Password" Nothing
  <*> pure []

validateLen :: Text -> Bool
validateLen a =
  (T.length a) > 3

generateString :: IO Text
generateString = (T.pack . I.take 16 . randoms) <$> newStdGen

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
