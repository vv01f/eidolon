module Model where

import Yesod
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
import Text.Show (Show)
import System.FilePath (FilePath)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
