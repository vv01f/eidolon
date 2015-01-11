{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  recentMedia <- runDB $ selectList [] [Desc MediumTime, LimitTo 30]
  widgetLayout <- return $ widgetFile "default-widget"
  defaultLayout $ do
    setTitle "Eidolon :: Home"
    $(widgetFile "home")
