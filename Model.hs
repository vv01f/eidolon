module Model where

import ClassyPrelude.Yesod
import Yesod.Markdown (Markdown)
import Data.Text (Text)
import Database.Persist.Quasi
import Database.Persist
import Database.Persist.TH
import Data.Typeable (Typeable)
import Data.Eq (Eq)
import Data.Time (UTCTime)
import Data.ByteString
import Data.Bool
import Data.Int
import Text.Show (Show)
import qualified System.FilePath as FP

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
