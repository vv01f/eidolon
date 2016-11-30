module Activate where

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
    activate <- select "#activate"
    on "click" (onActivateClick activate) activate

onActivateClick
  :: forall eff. JQuery
  -> JQueryEvent
  -> JQuery
  -> Eff (dom :: DOM
    , console :: CONSOLE
    , ajax :: AJAX
    | eff
    ) Unit
onActivateClick activate ev _ = unsafePartial $ do
  preventDefault ev
  token <- getAttr "data-token" activate
  salt <- getAttr "data-salt" activate
  fpassword1 <- getValue =<< select "#password1"
  fpassword2 <- getValue =<< select "#password2"
  let password1 = fromForeign fpassword1
      password2 = fromForeign fpassword2
  if not (isNothing password1 && isNothing password2)
    then
      if password1 == password2
        then do
          progress "Salting password"
          let salted = runFn2 hmacSha3 (fromJust password1) salt
          progress "Requesting account activation..."
          let dat = UE.encode (UE.fromArray [ Tuple "salted" (Just salted) ]) :: String
          _ <- runAff
              (\e -> fail Activate $ show e)
              (\x -> welcome x.response Activate)
              $ post ("/activate/" <> token) dat
          pure unit
        else
          fail Activate "Passwords must match"
    else
      fail Activate "Passwords error"
