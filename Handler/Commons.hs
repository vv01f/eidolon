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

module Handler.Commons where

import Import
import qualified Data.Text as T
import Data.String
import qualified Data.List as L
import System.FilePath as FP
import System.Directory

loginIsAdmin :: IsString t => Handler (Either (t, Route App)  ())
loginIsAdmin = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      let userId = getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      if
        userAdmin user
        then
          return $ Right ()
        else
          return $ Left ("You have no admin rights", HomeR)
    Nothing ->
      return $ Left ("You are not logged in", LoginR)

profileCheck :: IsString t => UserId -> Handler (Either (t, Route App) User)
profileCheck userId = do
  tempUser <- runDB $ get userId
  case tempUser of
    Just user -> do
      msu <- lookupSession "userId"
      case msu of
        Just tempLoginId -> do
          let loginId = getUserIdFromText tempLoginId
          if
            loginId == userId
            then
              return $ Right user
            else
              return $ Left ("You can only change your own profile settings", UserR $ userName user)
        Nothing ->
          return $ Left ("You nedd to be logged in to change settings", LoginR)
    Nothing ->
      return $ Left ("This user does not exist", HomeR)

mediumCheck :: IsString t => MediumId -> Handler (Either (t, Route App) Medium)
mediumCheck mediumId = do
  tempMedium <- runDB $ get mediumId
  case tempMedium of
    Just medium -> do
      let ownerId = mediumOwner medium
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          let userId = getUserIdFromText tempUserId
          album <- runDB $ getJust $ mediumAlbum medium
          let presence = userId == ownerId
          let albumOwnerPresence = userId == albumOwner album
          if
            presence || albumOwnerPresence
            then
              return $ Right medium
            else
              return $ Left ("You must own this medium to change its settings", MediumR mediumId)
        Nothing ->
          return $ Left ("You must be logged in to change settings", LoginR)
    Nothing ->
      return $ Left ("This medium does not exist", HomeR)

insertMedium :: Medium -> AlbumId -> Handler ()
insertMedium medium aId = do
  mId <- runDB $ insert medium
  inAlbum <- runDB $ getJust aId
  let newMediaList = mId : albumContent inAlbum
  runDB $ update aId [AlbumContent =. newMediaList]

deleteMedium :: MediumId -> Medium -> Handler ()
deleteMedium mId medium = do
  commEnts <- runDB $ selectList [CommentOrigin ==. mId] []
  -- delete comments first
  mapM_ (runDB . delete . entityKey) commEnts
  -- remove reference
  removeReference mId $ mediumAlbum medium
  -- delete Files
  mapM_ (liftIO . removeFile . normalise . L.tail)
    [ mediumPath medium
    , mediumThumb medium
    , mediumPreview medium
    ]
  -- delete database entry
  runDB $ delete mId

moveMedium :: Medium -> MediumId -> AlbumId -> Handler ()
moveMedium med mId destId = do
  $(logError) "getting destination"
  dest <- runDB $ getJust destId
  -- remove reference
  $(logError) "removing reference"
  removeReference mId $ mediumAlbum med
  -- move physical Files
  let filen = show $ length (albumContent dest) + 1
      ext   = takeExtension $ mediumPath med
      prefix = "static" </> "data" </> T.unpack (extractKey $ albumOwner dest) </> T.unpack (extractKey destId)
      nPath = prefix </> filen ++ ext
      nThumb = prefix </> takeBaseName nPath ++ "_thumb.jpg"
      nPrev = prefix </> takeBaseName nPath ++ "_preview.jpg"
  $(logError) $ T.pack $ "copyFile" ++ nPath
  liftIO $ copyFile (L.tail $ mediumPath med) nPath
  liftIO $ copyFile (L.tail $ mediumThumb med) nThumb
  liftIO $ copyFile (L.tail $ mediumPreview med) nPrev
  -- remove physical files
  $(logError) "removeFile"
  mapM_ (liftIO . removeFile . normalise . L.tail)
    [ mediumPath med
    , mediumThumb med
    , mediumPreview med
    ]
  -- chenge filenames in database
  runDB $ update mId
    [ MediumPath =. '/' : nPath
    , MediumThumb =. '/' : nThumb
    , MediumPreview =. '/' : nPrev
    , MediumAlbum =. destId
    ]
  -- create new references
  let newMediaList = mId : albumContent dest
  runDB $ update destId [AlbumContent =. newMediaList]

removeReference :: MediumId -> AlbumId -> Handler ()
removeReference mId aId = do
  -- delete references next
  album <- runDB $ getJust aId
  let newMediaList = removeItem mId $ albumContent album
  -- update reference list
  runDB $ update aId [AlbumContent =. newMediaList]
