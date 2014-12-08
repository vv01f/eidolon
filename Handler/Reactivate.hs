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
  ((result, reactivateWidget), enctype) <- runFormPost reactivateForm
  case result of
    FormSuccess temp -> do
      users <- runDB $ selectList [UserEmail ==. temp] []
      case null users of
        True -> do
          userTokens <- foldM (\userTokens (Entity userId user) -> do
            token <- liftIO $ generateString
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
