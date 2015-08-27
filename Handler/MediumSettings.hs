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

module Handler.MediumSettings where

import Import
import Handler.Commons
import System.Directory
import System.FilePath
import qualified Data.Text as T
import Data.List (tail)

getMediumSettingsR :: MediumId -> Handler Html
getMediumSettingsR mediumId = do
  checkRes <- mediumCheck mediumId
  case checkRes of
    Right medium -> do
      (mediumSettingsWidget, enctype) <- generateFormPost $ mediumSettingsForm medium
      formLayout $ do
        setTitle "Eidolon :: Medium Settings"
        $(widgetFile "mediumSettings")
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect $ route

postMediumSettingsR :: MediumId -> Handler Html
postMediumSettingsR mediumId = do
  checkRes <- mediumCheck mediumId
  case checkRes of
    Right medium -> do
      ((result, _), _) <- runFormPost $ mediumSettingsForm medium
      case result of
        FormSuccess temp -> do
          _ <- runDB $ update mediumId
            [ MediumTitle =. mediumTitle temp
            , MediumDescription =. mediumDescription temp
            , MediumTags =. mediumTags temp
            ]
          setMessage "Medium settings changed succesfully"
          redirect $ MediumR mediumId
        _ -> do
          setMessage "There was an error changing the settings"
          redirect $ MediumSettingsR mediumId
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect $ route

mediumSettingsForm :: Medium -> Form Medium
mediumSettingsForm medium = renderDivs $ Medium
  <$> areq textField "Title" (Just $ mediumTitle medium)
  <*> pure (mediumPath medium)
  <*> pure (mediumThumb medium)
  <*> pure (mediumMime medium)
  <*> pure (mediumTime medium)
  <*> pure (mediumOwner medium)
  <*> aopt textareaField "Description" (Just $ mediumDescription medium)
  <*> areq tagField "tags" (Just $ mediumTags medium)
  <*> pure (mediumWidth medium)
  <*> pure (mediumThumbWidth medium)
  <*> pure (mediumAlbum medium)

getMediumDeleteR :: MediumId -> Handler Html
getMediumDeleteR mediumId = do
  checkRes <- mediumCheck mediumId
  case checkRes of
    Right medium -> do
      formLayout $ do
        setTitle "Eidolon :: Delete Medium"
        $(widgetFile "mediumDelete")
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect $ route

postMediumDeleteR :: MediumId -> Handler Html
postMediumDeleteR mediumId = do
  checkRes <- mediumCheck mediumId
  case checkRes of
    Right medium -> do
      confirm <- lookupPostParam "confirm"
      case confirm of
        Just "confirm" -> do
          -- delete comments
          commEnts <- runDB $ selectList [CommentOrigin ==. mediumId] []
          _ <- mapM (\ent -> runDB $ delete $ entityKey ent) commEnts
          -- delete references first
          albumId <- return $ mediumAlbum medium
          album <- runDB $ getJust albumId
          mediaList <- return $ albumContent album
          newMediaList <- return $ removeItem mediumId mediaList
          -- update reference List
          runDB $ update albumId [AlbumContent =. newMediaList]
          liftIO $ removeFile (normalise $ tail $ mediumPath medium)
          liftIO $ removeFile (normalise $ tail $ mediumThumb medium)
          runDB $ delete mediumId
          setMessage "Medium succesfully deleted"
          redirect $ HomeR
        _ -> do
          setMessage "You must confirm the deletion"
          redirect $ MediumSettingsR mediumId
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect $ route
