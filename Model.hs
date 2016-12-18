--  eidolon -- A simple gallery in Haskell and Yesod
--  Copyright (C) 2015  Amedeo Molnár
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Affero General Public License as published
--  by the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Affero General Public License for more details.
--
--  You should have received a copy of the GNU Affero General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Yesod.Markdown
import qualified System.FilePath as FP

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

data Licence
  = AllRightsReserved
  | CC0
  | CCBy4
  | CCBySa4
  | CCByNd4
  | CCByNc4
  | CCByNcSa4
  | CCByNcNd4
  | PublicDomain

instance Show Licence where
  show AllRightsReserved = "All rights reserved"
  show CC0               = "CC0 1.0"
  show CCBy4             = "CC-BY 4.0 International"
  show CCBySa4           = "CC-BY-SA 4.0 International"
  show CCByNd4           = "CC-BY-ND 4.0 International"
  show CCByNc4           = "CC-BY-NC 4.0 International"
  show CCByNcSa4         = "CC-BY-NC-SA 4.0 International"
  show CCByNcNd4         = "CC-BY-NC-ND 4.0 International"
  show PublicDomain      = "Public Domain"

instance Enum Licence where
  fromEnum PublicDomain      = -2
  fromEnum AllRightsReserved = -1
  fromEnum CC0               = 0
  fromEnum CCBy4             = 1
  fromEnum CCBySa4           = 2
  fromEnum CCByNd4           = 3
  fromEnum CCByNc4           = 4
  fromEnum CCByNcSa4         = 5
  fromEnum CCByNcNd4         = 6
  fromEnum n                 = error $ "no licence no. for" ++ show n

  toEnum (-2) = PublicDomain
  toEnum (-1) = AllRightsReserved
  toEnum    0 = CC0
  toEnum    1 = CCBy4
  toEnum    2 = CCBySa4
  toEnum    3 = CCByNd4
  toEnum    4 = CCByNc4
  toEnum    5 = CCByNcSa4
  toEnum    6 = CCByNcNd4
  toEnum n    = error $ "No licence no. " ++ show n
