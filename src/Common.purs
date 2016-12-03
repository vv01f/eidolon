module Common where

import Prelude (Unit, bind, pure, ($), (<$>), (=<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (JQuery, display, select, setProp, setText)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Function.Uncurried (Fn2)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.Foreign as F
import Data.Argonaut (class DecodeJson, decodeJson, (.?), Json) as A
import Data.Argonaut.Decode

data Elem
  = Login
  | Activate

welcome :: forall eff. A.Json -> Elem -> Eff (dom :: DOM | eff) Unit
welcome j elem = do
  let eitherWelcome = decodeJson j
  case eitherWelcome of
    Left _ -> do
      let eitherErr = decodeJson j
      either (fail elem) (\(Err e) -> fail elem e.err) eitherErr
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

data Err = Err
  { err :: String
  }

instance decodeJsonErr :: DecodeJson Err where
  decodeJson j = do
    obj <- decodeJson j
    error <- obj .? "error"
    pure $ Err { err: error }

fromForeign :: F.Foreign -> Maybe String
fromForeign f = case runExcept (F.readString f) of
  Left e -> Nothing
  Right s -> Just s

foreign import hmacSha3 :: Fn2 WordArray WordArray String

foreign import setLocation :: forall eff. String -> Eff (dom :: DOM | eff) Unit

foreign import getAttr
  :: forall eff
   . String
  -> JQuery
  -> Eff (dom :: DOM | eff) String

foreign import toHex :: String -> String

foreign import fromHex :: String -> WordArray

foreign import fromUtf8 :: String -> WordArray

foreign import data WordArray :: *
