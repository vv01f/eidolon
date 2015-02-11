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

{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import qualified Data.Text as T

getHomeR :: Handler Html
getHomeR = do
  recentMedia <- runDB $ selectList [] [Desc MediumTime, LimitTo 30]
  nextMediaQuery <- runDB $ selectList [] [Desc MediumTime, LimitTo 1, OffsetBy 30]
  nextMedia <- return $ not $ null nextMediaQuery
  widgetLayout <- return $ widgetFile "default-widget"
  defaultLayout $ do
    setTitle "Eidolon :: Home"
    $(widgetFile "home")

getPageR :: Int -> Handler Html
getPageR page = do
  pageMedia <- runDB $ selectList [] [Desc MediumTime, LimitTo 30, OffsetBy (page*30)]
  nextMediaQuery <- runDB $ selectList [] [Desc MediumTime, LimitTo 1, OffsetBy ((page + 1) * 30)]
  nextMedia <- return $ not $ null nextMediaQuery
  widgetLayout <- return $ widgetFile "default-widget"
  defaultLayout $ do
    setTitle $ toHtml ("Eidolon :: Page " `T.append` (T.pack $ show page))
    $(widgetFile "page")
