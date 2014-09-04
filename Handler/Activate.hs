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
      mToken <- runDB $ selectFirst [TokenToken ==. (encodeUtf8 token), TokenKind ==. "activate"] []
      case mToken of
        Just (Entity uTokenId uToken) -> do
          user <- runDB $ getJust (fromJust $ tokenUser uToken)
          hexSalt <- return $ toHex $ userSalt user
          defaultLayout $ do
            $(widgetFile "activate")
        _ -> do
          setMessage "Invalid token!"
          redirect $ HomeR
    Just (Entity activatorKey activator) -> do
      uSalt <- return $ userSalt $ activatorUser activator
      mToken <- runDB $ selectFirst [TokenToken ==. (encodeUtf8 token), TokenKind ==. "activate"] []
      case mToken of
        Just (Entity uTokenId uToken) -> do
          hexSalt <- return $ toHex uSalt
          defaultLayout $ do
            $(widgetFile "activate")
        _ -> do
          setMessage "Invalid token!"
          redirect $ HomeR

postActivateR :: Text -> Handler RepJson
postActivateR token = do
  msalted <- fromJust <$> lookupPostParam "salted"
  salted <- return $ fromHex' $ unpack msalted
  mToken <- runDB $ selectFirst [TokenToken ==. (encodeUtf8 token), TokenKind ==. "activate"] []
  case mToken of
    Just (Entity uTokenId uToken) -> do
      case tokenUser uToken == Nothing of
        True -> do
          newUser <- runDB $ selectFirst [ActivatorToken ==. token] []
          case newUser of
            Just (Entity aId activ) -> do
              -- putting user in active state
              uId <- runDB $ insert $ activatorUser activ
              runDB $ update uId [UserSalted =. salted]
              -- create user directory
              liftIO $ createDirectoryIfMissing True $ "static" </> "data" </> (unpack $ extractKey uId)
              -- cleanup
              runDB $ delete aId
              runDB $ delete uTokenId
              -- login and redirect
              setSession "userId" (extractKey uId)
              welcomeLink <- ($ ProfileR uId) <$> getUrlRender
              returnJson ["welcome" .= welcomeLink]
            Nothing -> do
              returnJsonError "Invalid token"
        False -> do
          runDB $ update (fromJust $ tokenUser uToken) [UserSalted =. salted]
          -- cleanup
          runDB $ delete uTokenId
          setSession "userId" (extractKey $ fromJust $ tokenUser uToken)
          welcomeLink <- ($ ProfileR (fromJust $ tokenUser uToken)) <$> getUrlRender
          returnJson ["welcome" .= welcomeLink]
    _ -> do
      returnJsonError "Invalid activation token!"

returnJson :: (Monad m, ToJSON a, a ~ Value) =>
              [(Text, a)] -> m RepJson
returnJson = return . repJson . object

returnJsonError :: Text -> Handler RepJson
returnJsonError = returnJson . (:[]) . ("error" .=) 
