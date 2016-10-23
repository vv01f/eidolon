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
import System.FilePath
import qualified Data.Text as T
import Data.Maybe (catMaybes)

getMediumSettingsR :: MediumId -> Handler Html
getMediumSettingsR mediumId = do
  checkRes <- mediumCheck mediumId
  case checkRes of
    Right medium -> do
      (mediumSettingsWidget, enctype) <- generateFormPost $
        renderBootstrap3 BootstrapBasicForm $
        mediumSettingsForm medium
      defaultLayout $ do
        setTitle "Eidolon :: Medium Settings"
        $(widgetFile "mediumSettings")
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route

postMediumSettingsR :: MediumId -> Handler Html
postMediumSettingsR mediumId = do
  checkRes <- mediumCheck mediumId
  case checkRes of
    Right medium -> do
      ((result, _), _) <- runFormPost $
        renderBootstrap3 BootstrapBasicForm $
        mediumSettingsForm medium
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
      redirect route

mediumSettingsForm :: Medium -> AForm Handler Medium
mediumSettingsForm medium = Medium
  <$> areq textField (bfs ("Title" :: T.Text)) (Just $ mediumTitle medium)
  <*> pure (mediumPath medium)
  <*> pure (mediumThumb medium)
  <*> pure (mediumMime medium)
  <*> pure (mediumTime medium)
  <*> pure (mediumOwner medium)
  <*> aopt textareaField (bfs ("Description" :: T.Text)) (Just $ mediumDescription medium)
  <*> areq tagField (bfs ("tags" :: T.Text)) (Just $ mediumTags medium)
  <*> pure (mediumAlbum medium)
  <*> pure (mediumPreview medium)
  <*  bootstrapSubmit ("Change settings" :: BootstrapSubmit T.Text)

getMediumDeleteR :: MediumId -> Handler Html
getMediumDeleteR mediumId = do
  checkRes <- mediumCheck mediumId
  case checkRes of
    Right medium ->
      defaultLayout $ do
        setTitle "Eidolon :: Delete Medium"
        $(widgetFile "mediumDelete")
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route

postMediumDeleteR :: MediumId -> Handler Html
postMediumDeleteR mediumId = do
  checkRes <- mediumCheck mediumId
  case checkRes of
    Right medium -> do
      confirm <- lookupPostParam "confirm"
      case confirm of
        Just "confirm" -> do
          -- -- delete comments
          -- commEnts <- runDB $ selectList [CommentOrigin ==. mediumId] []
          -- _ <- mapM (runDB . delete . entityKey) commEnts
          -- -- delete references first
          -- let albumId = mediumAlbum medium
          -- album <- runDB $ getJust albumId
          -- let mediaList = albumContent album
          -- let newMediaList = removeItem mediumId mediaList
          -- -- update reference List
          -- runDB $ update albumId [AlbumContent =. newMediaList]
          -- liftIO $ removeFile (normalise $ tail $ mediumPath medium)
          -- liftIO $ removeFile (normalise $ tail $ mediumThumb medium)
          -- liftIO $ removeFile (normalise $ tail $ mediumPreview medium)
          -- runDB $ delete mediumId
          deleteMedium mediumId medium
          setMessage "Medium succesfully deleted"
          redirect HomeR
        _ -> do
          setMessage "You must confirm the deletion"
          redirect $ MediumSettingsR mediumId
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route

getMediumMoveR :: MediumId -> Handler Html
getMediumMoveR mId = do
  checkRes <- mediumCheck mId
  case checkRes of
    Right medium -> do
      (mediumMoveWidget, enctype) <- generateFormPost $
        renderBootstrap3 BootstrapBasicForm $
        mediumMoveForm medium
      defaultLayout $ do
        setTitle "Eidolon :: Move Medium"
        $(widgetFile "mediumMove")
    Left (err, route) -> do
      setMessage err
      redirect route

postMediumMoveR :: MediumId -> Handler Html
postMediumMoveR mId = do
  checkRes <- mediumCheck mId
  case checkRes of
    Right medium -> do
      ((res, _), _) <- runFormPost $
        renderBootstrap3 BootstrapBasicForm $
        mediumMoveForm medium
      case res of
        FormSuccess aId -> do
          moveMedium medium mId aId
          setMessage "Medium successfully moved"
          redirect $ MediumR mId
        _ -> do
          setMessage "Error moving image"
          redirect $ mediumR mId
    Left (err, route) -> do
      setMessage err
      redirect route

mediumMoveForm :: Medium -> AForm Handler AlbumId
mediumMoveForm medium = id
  <$> areq (selectField albums) (bfs ("Destination album" :: T.Text)) (Just $ mediumAlbum medium)
  <*  bootstrapSubmit ("Move medium" :: BootstrapSubmit Text)
  where
    albums = do
      allEnts <- runDB $ selectList [] [Asc AlbumTitle]
      ents <- return $ catMaybes $ map (\ent ->
        if uId == albumOwner (entityVal ent) || uId `elem` albumShares (entityVal ent)
          then Just ent
          else Nothing
        ) allEnts
      optionsPairs $ map (\alb -> ((albumTitle $ entityVal alb), entityKey alb)) ents
    uId = mediumOwner medium
