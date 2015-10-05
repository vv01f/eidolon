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
import Data.Time
import Data.Maybe
import qualified Data.Text as T
import Data.List as L
import qualified System.FilePath as FP
import Filesystem.Path.CurrentOS
import Graphics.ImageMagick.MagickWand
import Text.Blaze.Internal

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
              (dUploadWidget, enctype) <- generateFormPost $ dUploadForm userId albumId
              formLayout $ do
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
          if
            userId == ownerId || userId `elem` albumShares album
            -- is the logged in user the owner or is the album shared with him
            then do
              ((result, _), _) <- runFormPost (dUploadForm userId albumId)
              case result of
                FormSuccess temp -> do
                  let fils = fileBulkFiles temp
                  let indFils = zip [1..] fils
                  errNames <- mapM
                    (\(index, file) -> do
                      let mime = fileContentType file
                      if
                        mime `elem` acceptedTypes
                        then do
                          path <- writeOnDrive file ownerId albumId
                          (thumbPath, prevPath, iWidth, tWidth, pWidth) <- generateThumb path ownerId albumId
                          tempName <- if
                            length indFils == 1
                            then return $ fileBulkPrefix temp
                            else return (fileBulkPrefix temp `T.append` " " `T.append` T.pack (show (index :: Int)) `T.append` " of " `T.append` T.pack (show (length indFils)))
                          let medium = Medium tempName ('/' : path) ('/' : thumbPath) mime (fileBulkTime temp) (fileBulkOwner temp) (fileBulkDesc temp) (fileBulkTags temp) iWidth tWidth albumId ('/' : prevPath) pWidth
                          mId <- runDB $ I.insert medium
                          inALbum <- runDB $ getJust albumId
                          let newMediaList = mId : albumContent inALbum
                          runDB $ update albumId [AlbumContent =. newMediaList]
                          return Nothing
                        else
                          return $ Just $ fileName file
                      ) indFils
                  let onlyErrNames = removeItem Nothing errNames
                  if
                    L.null onlyErrNames
                    then do
                      setMessage "All images succesfully uploaded"
                      redirect HomeR
                    else do
                      let justErrNames = map fromJust onlyErrNames
                      let msg = Content $ Text $ "File type not supported of: " `T.append` T.intercalate ", " justErrNames
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

generateThumb :: FP.FilePath -> UserId -> AlbumId -> Handler (FP.FilePath, FP.FilePath, Int, Int, Int)
generateThumb path userId albumId = do
  let newName = FP.takeBaseName path ++ "_thumb.jpg"
  let newPath = "static" FP.</> "data" FP.</> T.unpack (extractKey userId) FP.</> T.unpack (extractKey albumId) FP.</> newName
  let prevName = FP.takeBaseName path ++ "_preview.jpg"
  let prevPath = "static" FP.</> "data" FP.</> T.unpack (extractKey userId) FP.</> T.unpack (extractKey albumId) FP.</> prevName
  (iWidth, tWidth, pWidth) <- liftIO $ withMagickWandGenesis $ do
    (_, w) <- magickWand
    (_, p) <- magickWand
    readImage w (decodeString path)
    readImage p (decodeString path)
    w1 <- getImageWidth w
    h1 <- getImageHeight w
    let h2 = 230
    let w2 = floor (fromIntegral w1 / fromIntegral h1 * fromIntegral h2 :: Double)
    let w3 = 1380
    let h3 = floor (fromIntegral h1 / fromIntegral w1 * fromIntegral w3 :: Double)
    setImageAlphaChannel w deactivateAlphaChannel
    setImageAlphaChannel p deactivateAlphaChannel
    setImageFormat w "jpeg"
    setImageFormat p "jpeg"
    resizeImage w w2 h2 lanczosFilter 1
    resizeImage p w3 h3 lanczosFilter 1
    setImageCompressionQuality w 95
    setImageCompressionQuality p 95
    writeImage w (Just (decodeString newPath))
    writeImage p (Just (decodeString prevPath))
    return (w1, w2, w3)
  return (newPath, prevPath, iWidth, tWidth, pWidth)

writeOnDrive :: FileInfo -> UserId -> AlbumId -> Handler FP.FilePath
writeOnDrive fil userId albumId = do
  --filen <- return $ fileName fil
  album <- runDB $ getJust albumId
  let filen = show $ length (albumContent album) + 1
  let ext = FP.takeExtension $ T.unpack $ fileName fil
  let path = "static" FP.</> "data" FP.</> T.unpack (extractKey userId) FP.</> T.unpack (extractKey albumId) FP.</> filen ++ ext
  liftIO $ fileMove fil path
  return path

dUploadForm :: UserId -> AlbumId -> Form FileBulk
dUploadForm userId albumId = renderDivs $ FileBulk
  <$> areq textField "Title" Nothing
  <*> areq multiFileField "Select file(s)" Nothing
  <*> lift (liftIO getCurrentTime)
  <*> pure userId
  <*> aopt textareaField "Description" Nothing
  <*> areq tagField "Enter tags" Nothing
  <*> pure albumId

data FileBulk = FileBulk
  { fileBulkPrefix :: Text
  , fileBulkFiles :: [FileInfo]
  , fileBulkTime :: UTCTime
  , fileBulkOwner :: UserId
  , fileBulkDesc :: Maybe Textarea
  , fileBulkTags :: [Text]
  , fileBulkAlbum :: AlbumId
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
          (uploadWidget, enctype) <- generateFormPost (bulkUploadForm userId)
          formLayout $ do
            setTitle "Eidolon :: Upload Medium"
            $(widgetFile "bulkUpload")
        else do
          setMessage "Please create an album first"
          redirect NewAlbumR
    Nothing -> do
      setMessage "You need to be logged in"
      redirect LoginR

bulkUploadForm :: UserId -> Form FileBulk
bulkUploadForm userId = renderDivs $ (\a b c d e f g -> FileBulk b c d e f g a)
  <$> areq (selectField albums) "Album" Nothing
  <*> areq textField "Title" Nothing
  <*> areq multiFileField "Select file(s)" Nothing
  <*> lift (liftIO getCurrentTime)
  <*> pure userId
  <*> aopt textareaField "Description" Nothing
  <*> areq tagField "Enter tags" Nothing
  where
    albums = do
      allEnts <- runDB $ selectList [] [Desc AlbumTitle]
      let entities = map fromJust $ removeItem Nothing $ map (\ent ->
            if
              userId == albumOwner (entityVal ent) || userId `elem` albumShares (entityVal ent)
              then Just ent
              else Nothing
            ) allEnts
      optionsPairs $ I.map (\alb -> (albumTitle $ entityVal alb, entityKey alb)) entities

postUploadR :: Handler Html
postUploadR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      let userId = getUserIdFromText tempUserId
      ((result, _), _) <- runFormPost (bulkUploadForm userId)
      case result of
        FormSuccess temp -> do
          let fils = fileBulkFiles temp
          let indFils = zip [1..] fils
          errNames <- mapM
            (\(index, file) -> do
              let mime = fileContentType file
              if
                mime `elem` acceptedTypes
                then do
                  let inAlbumId = fileBulkAlbum temp
                  albRef <- runDB $ getJust inAlbumId
                  let ownerId = albumOwner albRef
                  path <- writeOnDrive file ownerId inAlbumId
                  (thumbPath, prevPath, iWidth, tWidth, pWidth) <- generateThumb path ownerId inAlbumId
                  tempName <- if
                    length indFils == 1
                    then return $ fileBulkPrefix temp
                    else return (fileBulkPrefix temp `T.append` " " `T.append` T.pack (show (index :: Int)) `T.append` " of " `T.append` T.pack (show (length indFils)))
                  let medium = Medium tempName ('/' : path) ('/' : thumbPath) mime (fileBulkTime temp) (fileBulkOwner temp) (fileBulkDesc temp) (fileBulkTags temp) iWidth tWidth inAlbumId ('/' : prevPath) pWidth
                  mId <- runDB $ I.insert medium
                  inALbum <- runDB $ getJust inAlbumId
                  let newMediaList = mId : albumContent inALbum
                  runDB $ update inAlbumId [AlbumContent =. newMediaList]
                  return Nothing
                else
                  return $ Just $ fileName file
              ) indFils
          let onlyErrNames = removeItem Nothing errNames
          if
            L.null onlyErrNames
            then do
              setMessage "All images succesfully uploaded"
              redirect HomeR
            else do
              let justErrNames = map fromJust onlyErrNames
              let msg = Content $ Text $ "File type not supported of: " `T.append` T.intercalate ", " justErrNames
              setMessage msg
              redirect HomeR
        _ -> do
          setMessage "There was an error uploading the file"
          redirect UploadR
    Nothing -> do
      setMessage "You need to be logged in"
      redirect LoginR
