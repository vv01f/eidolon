module Login where

import Prelude
import Control.Monad.Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery
import Control.Monad.Except

import DOM (DOM)

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe
import Data.Either
import Data.Tuple (Tuple(..))
import Data.Unit

import Data.Argonaut as A
import Data.Argonaut.Decode

import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM

import DOM.Node.Element

import Network.HTTP.Affjax

import Data.FormURLEncoded as UE

import Partial.Unsafe

import Common

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- main = do
--   log "Hello sailor!"

main :: forall eff. Eff ( ajax :: AJAX
                        , dom :: DOM
                        , console :: CONSOLE
                        | eff
                        ) Unit
main =
  ready $ do
    log "logging in"
    login <- select "#login"
    on "click" (onLoginClick login) login

onLoginClick
  :: forall eff. JQuery
  -> JQueryEvent
  -> JQuery
  -> Eff ( dom :: DOM
    , console :: CONSOLE
    , ajax :: AJAX
    | eff
    ) Unit
onLoginClick login ev x = unsafePartial $ do
  log "click!"
  preventDefault ev
  hide login
  setProp "disabled" true <$> select ".login input"
  fusername <- getValue =<< select "#username"
  fpassword <- getValue =<< select "#password"
  let username = fromForeign fusername
      password = fromForeign fpassword
  if not (isNothing username && isNothing password)
    then do
      progress "Obtaining Challenge..."
      let data1 = UE.encode (UE.fromArray $ [ Tuple "username" username ]) :: String
      _ <- runAff
          (\e -> fail Login $ show e)
          (\x -> resp1Success (fromJust password) x.response)
          $ post "/login" data1
      log "success so far"
    else
      log "error"

resp1Success :: forall eff. String
  -> A.Json
  -> Eff ( dom :: DOM
    , console :: CONSOLE
    , ajax :: AJAX
    | eff
    ) Unit
resp1Success pass j = do
  log $ show j
  let eitherChallenge = decodeJson j
  case eitherChallenge of
    Left e ->
      fail Login e
    Right (Challenge challenge) -> do
      progress "HMAC 1"
      let salted = runFn2 hmacSha3 pass challenge.salt
      progress "HMAC 2"
      let response = runFn2 hmacSha3 salted challenge.token
      progress "sending response"
      let data2 = UE.encode (UE.fromArray $ [ Tuple "token" (Just challenge.token), Tuple "response" (Just response) ]) :: String
      _ <- runAff
          (\e -> fail Login $ show e)
          (\x -> welcome x.response Login)
          $ post "/login" data2
      pure unit
