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

module Handler.Tag where

import Import
import qualified Data.Text as T
import System.FilePath

getTagR :: Text -> Handler Html
getTagR tag = do
  tempMedia <- runDB $ selectList [] [Desc MediumTitle]
  almostMedia <- mapM (\a ->
    if tag `elem` mediumTags (entityVal a)
      then return (Just a)
      else return Nothing
      ) tempMedia
  let media = removeItem Nothing almostMedia
  defaultLayout $ do
    setTitle $ toHtml ("Eidolon :: Tag " `T.append` tag)
    $(widgetFile "tagMedia")
