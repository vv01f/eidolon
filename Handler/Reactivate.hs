module Handler.Reactivate where

import Import
import Control.Monad
import Data.Text.Encoding

getReactivateR :: Handler Html
getReactivateR = do
  (reactivateWidget, enctype) <- generateFormPost reactivateForm
  defaultLayout $ do
    $(widgetFile "reactivate")

postReactivateR :: Handler Html
postReactivateR = do
  ((result, reactivateWidget), enctype) <- runFormPost reactivateForm
  case result of
    FormSuccess temp -> do
      tempUsers <- runDB $ selectList [UserEmail ==. temp] []
      case tempUsers of
        users -> do
          userTokens <- foldM (\userTokens (Entity userId user) -> do
            token <- liftIO $ generateString
--            aId <- runDB $ insert $ Activator token user
--            runDB $ delete userId
            tId <- runDB $ insert $ Token (encodeUtf8 token) "activate" (Just userId)
            return $ (user, token) : userTokens
            ) [] users
          sent <- foldM (\sent (user, token) ->
            case sent of
              False ->
                return False
              True -> do
                activateLink <- ($ ActivateR token) <$> getUrlRender
                sendMail (userEmail user) "Reset your password" $
                  [shamlet|
                    <h1>Welcome again to Eidolon
                    To reset your password visit the following link:
                    <a href="#{activateLink}">#{activateLink}

                    See you soon!
                  |]
                return True
            ) True userTokens
          setMessage "Your new passwort will arrive in your e-mail"
          redirect $ HomeR
        [] -> do
          setMessage "No user mith this Email"
          redirect $ LoginR

reactivateForm :: Form Text
reactivateForm = renderDivs $ (\a -> a)
  <$> areq emailField "Email"  Nothing
