module Activate where

import Prelude (Unit, bind, not, pure, show, unit, ($), (&&), (<>), (=<<), (==))
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery (JQuery, JQueryEvent, display, hide, getValue, on, preventDefault, ready, select)

import DOM (DOM)

import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Tuple (Tuple(..))

import Network.HTTP.Affjax (AJAX, post)

import Data.FormURLEncoded as UE

import Partial.Unsafe (unsafePartial)

import Common (Elem(..), fail, fromForeign, fromHex, fromUtf8, getAttr, hmacSha3, progress, welcome)

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
    log "activating"
    display =<< select ".js-hidden"
    hide =<< select ".noscript"
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
  -- log $ "token: " <> token
  salt <- getAttr "data-salt" activate
  -- log $ "salt: " <> salt
  fpassword1 <- getValue =<< select "#password1"
  fpassword2 <- getValue =<< select "#password2"
  let password1 = fromForeign fpassword1
      password2 = fromForeign fpassword2
  if not (isNothing password1 && isNothing password2)
    then
      if password1 == password2
        then do
          progress "Salting password"
          -- log $ fromJust password1 <> " " <> salt
          let salted = runFn2 hmacSha3 (fromUtf8 $ fromJust password1) (fromHex salt)
          -- log salted
          progress "Requesting account activation..."
          let dat = UE.fromArray [ Tuple "salted" (Just salted) ]
          _ <- runAff
              (\e -> fail Activate $ show e)
              (\x -> welcome x.response Activate)
              $ post ("/activate/" <> token) dat
          pure unit
        else
          fail Activate "Passwords must match"
    else
      fail Activate "Passwords error"
