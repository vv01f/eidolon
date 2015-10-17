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

module Handler.AdminMediumSettings where

import Import
import Handler.Commons
import System.FilePath
import System.Directory
import Data.List (tail)
import qualified Data.Text as T

getAdminMediaR :: Handler Html
getAdminMediaR = do
  adminCheck <- loginIsAdmin
  case adminCheck of
    Right _ -> do
      media <- runDB $ selectList [] [Asc MediumTitle]
      defaultLayout $ do
        setTitle "Administration: Media"
        $(widgetFile "adminMedia")
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route

getAdminMediumSettingsR :: MediumId -> Handler Html
getAdminMediumSettingsR mediumId = do
  adminCheck <- loginIsAdmin
  case adminCheck of
    Right _ -> do
      tempMedium <- runDB $ get mediumId
      case tempMedium of
        Just medium -> do
          (adminMediumSetWidget, enctype) <- generateFormPost $ adminMediumSetForm medium
          formLayout $ do
            setTitle "Administration: Medium Settings"
            $(widgetFile "adminMediumSet")
        Nothing -> do
          setMessage "This medium does not exist"
          redirect AdminR
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route

postAdminMediumSettingsR :: MediumId -> Handler Html
postAdminMediumSettingsR mediumId = do
  adminCheck <- loginIsAdmin
  case adminCheck of
    Right _ -> do
      tempMedium <- runDB $ get mediumId
      case tempMedium of
        Just medium -> do
          ((res, _), _) <- runFormPost $ adminMediumSetForm medium
          case res of
            FormSuccess temp -> do
              runDB $ update mediumId
                [ MediumTitle =. mediumTitle temp
                , MediumDescription =. mediumDescription temp
                , MediumTags =. mediumTags temp
                ]
              putIndexES $ ESMedium mediumId temp
              setMessage "Medium settings changed successfully"
              redirect AdminR
            _ -> do
              setMessage "There was an error while changing the settings"
              redirect $ AdminMediumSettingsR mediumId
        Nothing -> do
          setMessage "This medium does not exist"
          redirect AdminR
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route

adminMediumSetForm :: Medium -> Form Medium
adminMediumSetForm medium = renderDivs $ Medium
  <$> areq textField "Title" (Just $ mediumTitle medium)
  <*> pure (mediumPath medium)
  <*> pure (mediumThumb medium)
  <*> pure (mediumMime medium)
  <*> pure (mediumTime medium)
  <*> pure (mediumOwner medium)
  <*> aopt textareaField "Description" (Just $ mediumDescription medium)
  <*> areq tagField "Tags" (Just $ mediumTags medium)
  <*> pure (mediumWidth medium)
  <*> pure (mediumThumbWidth medium)
  <*> pure (mediumAlbum medium)
  <*> pure (mediumPreview medium)
  <*> pure (mediumPreviewWidth medium)

getAdminMediumDeleteR :: MediumId -> Handler Html
getAdminMediumDeleteR mediumId = do
  adminCheck <- loginIsAdmin
  case adminCheck of
    Right _ -> do
      tempMedium <- runDB $ get mediumId
      case tempMedium of
        Just medium -> do
          -- remove reference from album
          let albumId = mediumAlbum medium
          album <- runDB $ getJust albumId
          let mediaList = albumContent album
          let newMediaList = removeItem mediumId mediaList
          runDB $ update albumId [AlbumContent =. newMediaList]
          -- delete comments
          commEnts <- runDB $ selectList [CommentOrigin ==. mediumId] []
          _ <- mapM (\ent -> do
            children <- runDB $ selectList [CommentParent ==. (Just $ entityKey ent)] []
            _ <- mapM (\child -> do
              -- delete comment children
              deleteIndexES $ ESComment (entityKey child) (entityVal child)
              runDB $ delete $ entityKey child
              ) children
            deleteIndexES $ ESComment (entityKey ent) (entityVal ent)
            runDB $ delete $ entityKey ent) commEnts
          -- delete medium
          deleteIndexES $ ESMedium mediumId medium
          runDB $ delete mediumId
          -- delete files
          liftIO $ removeFile (normalise $ tail $ mediumPath medium)
          liftIO $ removeFile (normalise $ tail $ mediumThumb medium)
          -- outro
          setMessage "Medium deleted successfully"
          redirect AdminR
        Nothing -> do
          setMessage "This medium does not exist"
          redirect AdminR
    Left (errorMsg, route) -> do
      setMessage errorMsg
      redirect route
