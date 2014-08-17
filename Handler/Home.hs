{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  recentMedia <- runDB $ selectList [] [Desc MediumTime]
  defaultLayout $ do
    $(widgetFile "home")
