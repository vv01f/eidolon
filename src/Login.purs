module Login where

import Prelude (Unit, bind, not, pure, show, unit, ($), (&&), (<$>), (<>), (=<<))
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery (JQuery, JQueryEvent, getValue, hide, on, preventDefault, ready, select, setProp)

import DOM (DOM)

import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

import Data.Argonaut as A
import Data.Argonaut.Decode (decodeJson)

import Network.HTTP.Affjax (AJAX, post)

import Data.FormURLEncoded as UE

import Partial.Unsafe (unsafePartial)

import Common (Challenge(..), Elem(..), fail, fromForeign, fromHex, fromUtf8, hmacSha3, progress, welcome)

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
      let data1 = UE.fromArray $ [ Tuple "username" username ]
      _ <- runAff
          (\e -> fail Login $ show e)
          (\y -> resp1Success (fromJust password) y.response)
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
      let salted = runFn2 hmacSha3 (fromUtf8 pass) (fromHex challenge.salt)
      log $ "salted: " <> salted
      progress "HMAC 2"
      let response = runFn2 hmacSha3 (fromUtf8 salted) (fromUtf8 challenge.token)
      log $ "resp: " <> response
      progress "sending response"
      let data2 = UE.fromArray $ [ Tuple "token" (Just challenge.token), Tuple "response" (Just response) ]
      _ <- runAff
          (\e -> fail Login $ show e)
          (\x -> welcome x.response Login)
          $ post "/login" data2
      pure unit
