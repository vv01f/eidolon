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
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.module Handler.Activate where

module Settings.StaticFiles where

import qualified Yesod.Static as Static
import Settings (appStaticDir, compileTimeAppSettings)

-- | use this to create your static file serving site
-- staticSite :: IO Static.Static
-- staticSite = if development then Static.staticDevel staticDir
--                             else Static.static      staticDir

-- | This generates easy references to files in the static directory at compile time,
--   giving you compile-time verification that referenced files exist.
--   Warning: any files added to your static directory during run-time can't be
--   accessed this way. You'll have to use their FilePath or URL to access them.
Static.staticFiles (appStaticDir compileTimeAppSettings)

-- combineSettings :: CombineSettings
-- combineSettings = def
-- 
-- -- The following two functions can be used to combine multiple CSS or JS files
-- -- at compile time to decrease the number of http requests.
-- -- Sample usage (inside a Widget):
-- --
-- -- > $(combineStylesheets 'StaticR [style1_css, style2_css])
-- 
-- combineStylesheets :: Name -> [Route Static] -> Q Exp
-- combineStylesheets = combineStylesheets' development combineSettings
-- 
-- combineScripts :: Name -> [Route Static] -> Q Exp
-- combineScripts = combineScripts' development combineSettings
