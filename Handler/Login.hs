module Handler.Login where

import Import
import Data.Text

data Credentials = Credentials
  { credentialsName   :: Text
  , credentialsPasswd :: Text
  }
  deriving Show

getLoginR :: Handler Html
getLoginR = do
  (loginWidget, enctype) <- generateFormPost loginForm
  defaultLayout $ do
    $(widgetFile "login")

postLoginR :: Handler Html
postLoginR = do
  ((result, loginWidget), enctype) <- runFormPost loginForm
  case result of
    FormSuccess cred -> do
      tuser <- runDB $ selectFirst [UserName ==. credentialsName cred] []
      case tuser of
        Just user -> do
          case credentialsPasswd cred == userPassword (entityVal user) of
            True -> do
              setSession "userId" $ extractKey $ entityKey user
              setMessage "Successfully logged in"
              redirect $ HomeR
            False -> do
              setMessage $ "Login error"
              redirect $ LoginR
        Nothing -> do
          setMessage "User does not exist"
          redirect $ LoginR

loginForm :: Form Credentials
loginForm = renderDivs $ Credentials
  <$> areq textField "Username" Nothing
  <*> areq passwordField "Password" Nothing

getLogoutR :: Handler Html
getLogoutR = do
  deleteSession "userId"
  setMessage "Succesfully logged out"
  redirect $ HomeR
