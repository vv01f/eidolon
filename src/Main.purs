module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery
import Control.Monad.Except

import DOM (DOM)

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe
import Data.Either
import Data.Tuple (Tuple(..))
import Data.Foreign

import Data.Argonaut as A
import Data.Argonaut.Decode

import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM

import Network.HTTP.Affjax (get, post)

import Data.FormURLEncoded as UE

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- main = do
--   log "Hello sailor!"

main :: forall eff. Eff ( dom :: DOM
                        , console :: CONSOLE
                        | eff
                        ) Unit
main =
  ready $ do
    hide <$> select "#login"
    setProp "disabled" true <$> select ".login input"

    fusername <- getValue =<< select "#username"
    fpassword <- getValue =<< select "#password"

    let username = fromForeign fusername
        password = fromForeign fpassword

    if not (isNothing username && isNothing password)
      then do

        progress "Obtaining Challenge..."

        let data1 = UE.encode (UE.fromArray $ [ Tuple "username" username ]) :: String

        -- resp1 <- post "/login" data1
        -- (Challenge challenge) <- decodeJson resp1.response

        progress "HMAC 1"

        -- let salted = runFn2 hmacSha3 (fromJust password) challenge.salt

        log "bla"
      else
        log "error"

progress :: forall eff. String -> Eff (dom :: DOM | eff) Unit
progress s = do
  setText s =<< select "#progress"

fail :: forall eff. String -> Eff (dom :: DOM | eff) Unit
fail s = do
  progress s
  setProp "disabled" false <$> select ".login input"
  display =<< select "#login"

data Challenge = Challenge
  { salt :: String
  , token :: String
  }

instance decodeJsonChallenge :: DecodeJson Challenge where
  decodeJson j = do
    obj <- decodeJson j
    salt <- obj .? "salt"
    token <- obj .? "token"
    pure $ Challenge { salt, token }

fromForeign :: Foreign -> Maybe String
fromForeign f = case runExcept (readString f) of
  Left e -> Nothing
  Right s -> Just s

foreign import hmacSha3 :: Fn2 String String String
