{-# LANGUAGE TupleSections, OverloadedStrings #-} 
module Handler.Signup where

import Import as I
import Data.Text as T
import Data.Text.Encoding
import Data.Maybe

getSignupR :: Handler Html
getSignupR = do
--  (signupWidget, enctype) <- generateFormPost signupForm
  defaultLayout $ do
    setTitle "Eidolon :: Signup"
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
      newUser <- return $ User newUserName
        newUserName
        (fromJust mEmail)
        salt
        ""
        []
        False
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

validateLen :: Text -> Bool
validateLen a =
  (T.length a) > 3
