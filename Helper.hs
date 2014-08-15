module Helper 
  ( getUserIdFromText
  , extractKey
--  , getUserNameById
  )
where

import Prelude
import Model
import Control.Applicative
import Control.Monad.Trans.Class
import Data.Text
import Data.Maybe
import Database.Persist
import Database.Persist.Types
import System.FilePath
import Yesod.Persist.Core
import Yesod.Core.Types

getUserIdFromText :: Text -> UserId
getUserIdFromText tempUserId =
  Key $ PersistInt64 $ fromIntegral $ read $ unpack tempUserId

extractKey :: KeyBackend backend entity -> Text
extractKey = extractKey' . unKey
  where
    extractKey' (PersistInt64 k) = pack $ show k
    extractKey' _ = ""

--getUserNameById :: UserId -> Text
--getUserNameById userId =
--  let
--    user = runDB $ getJust $ userId
--  in
--    userName user
