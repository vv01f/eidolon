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
import Yesod.Text.Markdown
import Handler.Commons
import System.FilePath
import qualified Data.Text as T
import Data.Maybe
import Text.Markdown
import Control.Monad (when)

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
            [ MediumTitle =. msTitle temp
            , MediumDescription =. msDescription temp
            , MediumTags =. msTags temp
            , MediumLicence =. msLicence temp
            ]
          when (not $ isNothing $ msData temp) $ do
            err <- handleUpload
              1
              (mediumAlbum medium)
              (msTitle temp)
              (mediumTime medium)
              (mediumOwner medium)
              (msDescription temp)
              (msTags temp)
              (msLicence temp)
              (Replace mediumId)
              (1, (fromJust $ msData temp))
            when (not $ isNothing err) $ do
              setMessage "There was an error uploading the File"
              redirect $ MediumSettingsR mediumId
          setMessage "Medium settings changed succesfully"
          redirect $ MediumR mediumId
        _ -> do
          setMessage "There was an error changing the settings"
          redirect $ MediumSettingsR mediumId
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route

data MediumSettings = MediumSettings
  { msTitle       :: T.Text
  , msData        :: Maybe FileInfo
  , msDescription :: Maybe Markdown
  , msTags        :: [T.Text]
  , msLicence     :: Int
  }

mediumSettingsForm :: Medium -> AForm Handler MediumSettings
mediumSettingsForm medium = MediumSettings
  <$> areq textField (bfs ("Title" :: T.Text)) (Just $ mediumTitle medium)
  <*> aopt fileField (bfs ("Update medium" :: T.Text)) (Nothing)
  <*> aopt markdownField (bfs ("Description" :: T.Text)) (Just $ mediumDescription medium)
  <*> areq tagField (bfs ("Tags" :: T.Text)) (Just $ mediumTags medium)
  <*> areq (selectField licences) (bfs ("Licence" :: T.Text)) (Just $ mediumLicence medium)
  <*  bootstrapSubmit ("Change settings" :: BootstrapSubmit T.Text)
  where
    licences = optionsPairs $ map (\a -> (T.pack (show (toEnum a :: Licence)), a)) [-2..6]

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
          redirect $ MediumR mId
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
