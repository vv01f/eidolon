module Helper 
  ( getUserIdFromText
  , extractKey
  )
where

import Prelude
import Data.Text
import Database.Persist.Types

getUserIdFromText :: Text -> PersistValue
getUserIdFromText tempUserId =
  PersistInt64 $ fromIntegral $ read $ unpack tempUserId

extractKey :: KeyBackend backend entity -> Text
extractKey = extractKey' . unKey
  where
    extractKey' (PersistInt64 k) = pack $ show k
    extractKey' _ = ""
