module Handler.AlbumSettings where

import Import
import qualified Data.Text as T
import Data.Maybe
import System.Directory
import System.FilePath
import qualified Data.List as L

getAlbumSettingsR :: AlbumId -> Handler Html
getAlbumSettingsR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of
    Just album -> do
      ownerId <- return $ albumOwner album
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          ownerPresence <- return (userId == ownerId)
          presence <- return $ userId `elem` (albumShares album)
          case ownerPresence || presence of
            True -> do
              entities <- runDB $ selectList [UserId !=. (albumOwner album)] [Desc UserName]
              users <- return $ map (\u -> (userName $ entityVal u, entityKey u)) entities
              (albumSettingsWidget, enctype) <- generateFormPost $ albumSettingsForm album albumId users
              formLayout $ do
                setTitle "Eidolon :: Album Settings"
                $(widgetFile "albumSettings")
            False -> do
              setMessage "You must own this album to change its settings"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to change settings"
          redirect $ LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect $ HomeR

postAlbumSettingsR :: AlbumId -> Handler Html
postAlbumSettingsR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of
    Just album -> do
      ownerId <- return $ albumOwner album
      owner <- runDB $ getJust ownerId
      ownerName <- return $ userName owner
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          ownerPresence <- return (userId == ownerId)
          presence <- return $ userId `elem` (albumShares album)
          case ownerPresence || presence of
            True -> do
              entities <- runDB $ selectList [UserId !=. (albumOwner album)] [Desc UserName]
              users <- return $ map (\u -> (userName $ entityVal u, entityKey u)) entities
              ((result, _), _) <- runFormPost $ albumSettingsForm album albumId users
              case result of
                FormSuccess temp -> do
                  newShares <- return (L.sort $ albumShares temp)
                  oldShares <- return (L.sort $ albumShares album)
                  _ <- case newShares /= oldShares of
                    True -> do
                      link <- ($ AlbumR albumId) <$> getUrlRender
                      rcptIds <- return $ L.nub $ newShares L.\\ oldShares
                      mapM (\uId -> do
                        -- update userAlbums
                        user <- runDB $ getJust uId
                        oldAlbs <- return $ userAlbums user
                        newAlbs <- return $ albumId : oldAlbs
                        _ <- runDB $ update uId [UserAlbums =. newAlbs]
                        -- send notification
                        addr <- return $ userEmail user
                        sendMail addr "A new album was shared with you" $
                          [shamlet|
                            <h1>Hello #{userSlug user}!
                            <p>#{ownerName} was so kind to share his album #{albumTitle album} with you.
                            <p>You can find it
                              <a href=#{link}>
                                here
                              .
                            |]
                        ) rcptIds
                    False -> do
                      return [()]
                      -- nothing to do here
                  width <- getThumbWidth $ Just $ L.tail $ fromMaybe ['a'] $ albumSamplePic temp
                  _ <- runDB $ update albumId 
                    [ AlbumTitle =. albumTitle temp
                    , AlbumShares =. newShares
                    , AlbumSamplePic =. albumSamplePic temp
                    , AlbumSampleWidth =. width
                    ]
                  setMessage "Album settings changed succesfully"
                  redirect $ AlbumR albumId
                _ -> do
                  setMessage "There was an error while changing the settings"
                  redirect $ AlbumSettingsR albumId
            False -> do
              setMessage "You must own this album to change its settings"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to change settings"
          redirect $ LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect $ HomeR

albumSettingsForm :: Album -> AlbumId -> [(Text, UserId)]-> Form Album
albumSettingsForm album albumId users = renderDivs $ Album
  <$> areq textField "Title" (Just $ albumTitle album)
  <*> pure (albumOwner album)
  <*> areq (userField users) "Share this album with" (Just $ albumShares album)
  <*> pure (albumContent album)
  <*> aopt (selectField media) "Sample picture" (Just $ albumSamplePic album)
  <*> pure 230
  where
    media = do
      entities <- runDB $ selectList [MediumAlbum ==. albumId] [Asc MediumTitle]
      optionsPairs $ map (\med -> (mediumTitle $ entityVal med, mediumThumb (entityVal med))) entities
--    users = do
--      entities <- runDB $ selectList [UserId !=. (albumOwner album)] [Desc UserName]
--      return $ map (\u -> (userName $ entityVal u, entityKey u)) entities

getAlbumDeleteR :: AlbumId -> Handler Html
getAlbumDeleteR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of
    Just album -> do
      ownerId <- return $ albumOwner album
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          presence <- return (userId == ownerId)
          case presence of
            True -> do
              formLayout $ do
                setTitle $ toHtml ("Eidolon :: Delete album" `T.append` (albumTitle album))
                $(widgetFile "albumDelete")
            False -> do
              setMessage "You must own this album to delete it"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to delete albums"
          redirect $ LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect $ HomeR

postAlbumDeleteR :: AlbumId -> Handler Html
postAlbumDeleteR albumId = do
  tempAlbum <- runDB $ get albumId
  case tempAlbum of
    Just album -> do
      ownerId <- return $ albumOwner album
      owner <- runDB $ getJust ownerId
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          presence <- return (userId == ownerId)
          case presence of
            True -> do
              confirm <- lookupPostParam "confirm"
              case confirm of
                Just "confirm" -> do
                  -- remove album reference from user
                  albumList <- return $ userAlbums owner
                  newAlbumList <- return $ removeItem albumId albumList
                  runDB $ update ownerId [UserAlbums =. newAlbumList]
                  -- delete album content and its comments
                  _ <- mapM (\a -> do
                    -- delete files
                    medium <- runDB $ getJust a
                    liftIO $ removeFile (normalise $ L.tail $ mediumPath medium)
                    liftIO $ removeFile (normalise $ L.tail $ mediumThumb medium)
                    -- delete comments
                    commEnts <- runDB $ selectList [CommentOrigin ==. a] []
                    _ <- mapM (\ent -> runDB $ delete $ entityKey ent) commEnts
                    runDB $ delete a
                    ) (albumContent album)
                  -- delete album
                  runDB $ delete albumId
                  -- delete files
                  liftIO $ removeDirectoryRecursive $ "static" </> "data" </> (T.unpack $ extractKey userId) </> (T.unpack $ extractKey albumId)
                  -- outro
                  setMessage "Album deleted succesfully"
                  redirect $ HomeR
                _ -> do
                  setMessage "You must confirm the deletion"
                  redirect $ AlbumSettingsR albumId
            _ -> do
              setMessage "You must own this album to delete it"
              redirect $ AlbumR albumId
        Nothing -> do
          setMessage "You must be logged in to delete albums"
          redirect $ LoginR
    Nothing -> do
      setMessage "This album does not exist"
      redirect $ HomeR
