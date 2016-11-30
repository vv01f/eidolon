module Common where

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
import Data.Foreign as F
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

data Elem
  = Login
  | Activate

welcome :: forall eff. A.Json -> Elem -> Eff (dom :: DOM | eff) Unit
welcome j elem = do
  let eitherWelcome = decodeJson j
  case eitherWelcome of
    Left e ->
      fail elem e
    Right (Welcome welc) -> do
      progress "welcome to Eidolon"
      setLocation welc.link

progress :: forall eff. String -> Eff (dom :: DOM | eff) Unit
progress s = do
  setText s =<< select "#progress"

fail :: forall eff. Elem -> String -> Eff (dom :: DOM | eff) Unit
fail Login s = do
  progress s
  setProp "disabled" false <$> select ".login input"
  display =<< select "#login"
fail Activate s = do
  progress s
  setProp "disabled" false <$> select ".activate input"
  display =<< select "#activate"

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

data Welcome = Welcome
  { link :: String
  }

instance decodeJsonWelcome :: DecodeJson Welcome where
  decodeJson j = do
    obj <- decodeJson j
    link <- obj .? "welcome"
    pure $ Welcome { link }

fromForeign :: F.Foreign -> Maybe String
fromForeign f = case runExcept (F.readString f) of
  Left e -> Nothing
  Right s -> Just s

foreign import hmacSha3 :: Fn2 String String String

foreign import setLocation :: forall eff. String -> Eff (dom :: DOM | eff) Unit

foreign import getAttr
  :: forall eff a
   . String
  -> JQuery
  -> Eff (dom :: DOM | eff) String
