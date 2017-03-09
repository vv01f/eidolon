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

module Handler.Upload where

import Import as I
import Yesod.Text.Markdown
import Text.Markdown
import Handler.Commons
import Data.Time
import Data.Maybe
import qualified Data.Text as T
import Data.List as L
import qualified System.FilePath as FP
import System.Directory
import Text.Blaze.Internal
-- import Codec.ImageType
-- import Codec.Picture as P
-- import Codec.Picture.Metadata as PM
-- import Codec.Picture.ScaleDCT
import Graphics.Rasterific.Svg as SVG
-- import Graphics.Svg
-- import Graphics.Text.TrueType

import Debug.Trace

getDirectUploadR :: AlbumId -> Handler Html
getDirectUploadR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of -- does the requested album exist
    Just album -> do
      let ownerId = albumOwner album
      msu <- lookupSession "userId"
      case msu of -- is anybody logged in
        Just tempUserId -> do
          let userId = getUserIdFromText tempUserId
          if
            userId == ownerId || userId `elem` albumShares album
            -- is the owner present or a user with whom the album is shared
            then do
              user <- runDB $ getJust userId
              (dUploadWidget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ dUploadForm userId user albumId
              defaultLayout $ do
                setTitle $ toHtml ("Eidolon :: Upload medium to " `T.append` albumTitle album)
                $(widgetFile "dUpload")
            else do
              setMessage "You must own this album to upload"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to upload"
          redirect LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect HomeR

postDirectUploadR :: AlbumId -> Handler Html
postDirectUploadR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of -- does the album exist
    Just album -> do
      let ownerId = albumOwner album
      msu <- lookupSession "userId"
      case msu of -- is anybody logged in
        Just tempUserId -> do
          let userId = getUserIdFromText tempUserId
          if userId == ownerId || userId `elem` albumShares album
            -- is the logged in user the owner or is the album shared with him
            then do
              user <- runDB $ getJust userId
              ((result, _), _) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ dUploadForm userId user albumId
              case result of
                FormSuccess temp -> do
                  let fils = fileBulkFiles temp
                  let indFils = zip [1..] fils
                  errNames <- mapM
                    (handleUpload
                      (length indFils)
                      (fileBulkAlbum temp)
                      (fileBulkPrefix temp)
                      (fileBulkTime temp)
                      (fileBulkOwner temp)
                      (fileBulkDesc temp)
                      (fileBulkTags temp)
                      (fileBulkLicence temp)
                      NewFile
                    )indFils
                  let onlyErrNames = removeItem Nothing errNames
                  if
                    L.null onlyErrNames
                    then do
                      setMessage "All images succesfully uploaded"
                      redirect $ AlbumR albumId
                    else do
                      let justErrNames = map fromJust onlyErrNames
                      let msg = Content $ Text.Blaze.Internal.Text $ "File type not supported of: " `T.append` T.intercalate ", " justErrNames
                      setMessage msg
                      redirect HomeR
                _ -> do
                  setMessage "There was an error uploading the file"
                  redirect $ DirectUploadR albumId
            else do -- owner is not present
              setMessage "You must own this album to upload"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to upload"
          redirect $ AlbumR albumId
    Nothing -> do
      setMessage "This Album does not exist"
      redirect $ AlbumR albumId

dUploadForm :: UserId -> User -> AlbumId -> AForm Handler FileBulk
dUploadForm userId user albumId = FileBulk
  <$> areq textField (bfs ("Title" :: T.Text)) Nothing
  <*> areq multiFileField "Select file(s)" Nothing
  <*> lift (liftIO getCurrentTime)
  <*> pure userId
  <*> aopt markdownField (bfs ("Description" :: T.Text)) Nothing
  <*> areq tagField (bfs ("Enter tags" :: T.Text)) Nothing
  <*> pure albumId
  <*> areq (selectField licences) (bfs ("Licence" :: T.Text)) (defLicence)
  <*  bootstrapSubmit ("Upload" :: BootstrapSubmit T.Text)
  where
    licences = optionsPairs $ I.map (\a -> (T.pack (show (toEnum a :: Licence)), a)) [-2..6]
    defLicence = Just $ userDefaultLicence user
      

data FileBulk = FileBulk
  { fileBulkPrefix :: T.Text
  , fileBulkFiles :: [FileInfo]
  , fileBulkTime :: UTCTime
  , fileBulkOwner :: UserId
  , fileBulkDesc :: Maybe Markdown
  , fileBulkTags :: [T.Text]
  , fileBulkAlbum :: AlbumId
  , fileBulkLicence :: Int
  }

getUploadR :: Handler Html
getUploadR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      let userId = getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      let albums = userAlbums user
      if
        I.null albums
        then do
          setMessage "Please create an album first"
          redirect NewAlbumR
        else do
          (uploadWidget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ bulkUploadForm userId user
          defaultLayout $ do
            setTitle "Eidolon :: Upload Medium"
            $(widgetFile "bulkUpload")
    Nothing -> do
      setMessage "You need to be logged in"
      redirect LoginR

bulkUploadForm :: UserId -> User -> AForm Handler FileBulk
bulkUploadForm userId user = (\a b c d e f g h -> FileBulk b c d e f g a h)
  <$> areq (selectField albums) (bfs ("Album" :: T.Text)) Nothing
  <*> areq textField (bfs ("Title" :: T.Text)) Nothing
  <*> areq multiFileField "Select file(s)" Nothing
  <*> lift (liftIO getCurrentTime)
  <*> pure userId
  <*> aopt markdownField (bfs ("Description" :: T.Text)) Nothing
  <*> areq tagField (bfs ("Enter tags" :: T.Text)) Nothing
  <*> areq (selectField licences) (bfs ("Licence" :: T.Text)) (defLicence)
  <*  bootstrapSubmit ("Upload" :: BootstrapSubmit T.Text)
  where
    albums = do
      allEnts <- runDB $ selectList [] [Desc AlbumTitle]
      let entities = catMaybes $ map (\ent ->
            if
              userId == albumOwner (entityVal ent) || userId `elem` albumShares (entityVal ent)
              then Just ent
              else Nothing
            ) allEnts
      optionsPairs $ I.map (\alb -> (albumTitle $ entityVal alb, entityKey alb)) entities
    licences = optionsPairs $ I.map (\a -> (T.pack (show (toEnum a :: Licence)), a)) [-2..6]
    defLicence = Just $ userDefaultLicence user

postUploadR :: Handler Html
postUploadR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      let userId = getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      ((result, _), _) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ bulkUploadForm userId user
      case result of
        FormSuccess temp -> do
          let fils = fileBulkFiles temp
          let indFils = zip [1..] fils
          errNames <- mapM
            (handleUpload
              (length indFils)
              (fileBulkAlbum temp)
              (fileBulkPrefix temp)
              (fileBulkTime temp)
              (fileBulkOwner temp)
              (fileBulkDesc temp)
              (fileBulkTags temp)
              (fileBulkLicence temp)
              NewFile
            )indFils
          let onlyErrNames = removeItem Nothing errNames
          if
            L.null onlyErrNames
            then do
              setMessage "All images succesfully uploaded"
              redirect $ AlbumR $ fileBulkAlbum temp
            else do
              let justErrNames = map fromJust onlyErrNames
              let msg = Content $ Text.Blaze.Internal.Text $ "File type not supported of: " `T.append` T.intercalate ", " justErrNames
              setMessage msg
              redirect $ AlbumR $ fileBulkAlbum temp
        _ -> do
          setMessage "There was an error uploading the file(s)"
          redirect UploadR
    Nothing -> do
      setMessage "You need to be logged in"
      redirect LoginR
