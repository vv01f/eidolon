module Helper 
  ( getUserIdFromText
  )
where

import Prelude
import Data.Text
import Database.Persist.Types

getUserIdFromText :: Text -> PersistValue
getUserIdFromText tempUserId =
  PersistInt64 $ fromIntegral $ read $ unpack tempUserId
