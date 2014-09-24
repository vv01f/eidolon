module Handler.MediumSettings where

import Import
import System.Directory
import System.FilePath
import Data.List (tail)

getMediumSettingsR :: MediumId -> Handler Html
getMediumSettingsR mediumId = do
  tempMedium <- runDB $ get mediumId
  case tempMedium of
    Just medium -> do
      ownerId <- return $ mediumOwner medium
      owner <- runDB $ getJust ownerId
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          presence <- return (userId == ownerId)
          case presence of
            True -> do
              (mediumSettingsWidget, enctype) <- generateFormPost $ mediumSettingsForm medium
              defaultLayout $ do
                setTitle "Eidolon :: Medium Settings"
                $(widgetFile "mediumSettings")
            False -> do
              setMessage "You must own this medium to change its settings"
              redirect $ MediumR mediumId
        Nothing -> do
          setMessage "You must be logged in to change settings"
          redirect $ LoginR
    Nothing -> do
      setMessage "This medium does not exist"
      redirect $ HomeR

postMediumSettingsR :: MediumId -> Handler Html
postMediumSettingsR mediumId = do
  tempMedium <- runDB $ get mediumId
  case tempMedium of
    Just medium -> do
      ownerId <- return $ mediumOwner medium
      owner <- runDB $ getJust ownerId
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          presence <- return (userId == ownerId)
          case presence of
            True -> do
              ((result, mediumSettingsWidget), enctype) <- runFormPost $ mediumSettingsForm medium
              case result of
                FormSuccess temp -> do
                  mId <- runDB $ update mediumId
                    [ MediumTitle =. mediumTitle temp
                    , MediumDescription =. mediumDescription temp
                    , MediumTags =. mediumTags temp
                    ]
                  setMessage "Medium settings changed succesfully"
                  redirect $ MediumR mediumId
                _ -> do
                  setMessage "There was an error changing the settings"
                  redirect $ MediumSettingsR mediumId
            False -> do
              setMessage "You must own this medium to change its settings"
              redirect $ MediumR mediumId
        Nothing -> do
          setMessage "You must be logged in to change settings"
          redirect $ LoginR
    Nothing -> do
      setMessage "This medium does not exist"
      redirect $ HomeR

mediumSettingsForm :: Medium -> Form Medium
mediumSettingsForm medium = renderDivs $ Medium
  <$> areq textField "Title" (Just $ mediumTitle medium)
  <*> pure (mediumPath medium)
  <*> pure (mediumTime medium)
  <*> pure (mediumOwner medium)
  <*> areq textareaField "Description" (Just $ mediumDescription medium)
  <*> areq tagField "tags" (Just $ mediumTags medium)
  <*> pure (mediumAlbum medium)

getMediumDeleteR :: MediumId -> Handler Html
getMediumDeleteR mediumId = do
  tempMedium <- runDB $ get mediumId
  case tempMedium of
    Just medium -> do
      ownerId <- return $ mediumOwner medium
      owner <- runDB $ getJust ownerId
      msu <- lookupSession "userId"
      case msu of
        Just tempUserId -> do
          userId <- return $ getUserIdFromText tempUserId
          presence <- return (userId == ownerId)
          case presence of
            True -> do
              defaultLayout $ do
                setTitle "Eidolon :: Delete Medium"
                $(widgetFile "mediumDelete")
            False -> do
              setMessage "You must own this medium to delete it"
              redirect $ MediumR mediumId
        Nothing -> do
          setMessage "You must be logged in to delete media"
          redirect $ LoginR
    Nothing -> do
      setMessage "This Medium does not exist"
      redirect $ HomeR

postMediumDeleteR :: MediumId -> Handler Html
postMediumDeleteR mediumId = do
  tempMedium <- runDB $ get mediumId
  case tempMedium of
    Just medium -> do
      ownerId <- return $ mediumOwner medium
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
                  -- delete references first
                  albumId <- return $ mediumAlbum medium
                  album <- runDB $ getJust albumId
                  mediaList <- return $ albumContent album
                  newMediaList <- return $ removeItem mediumId mediaList
                  -- update reference List
                  runDB $ update albumId [AlbumContent =. newMediaList]
                  liftIO $ removeFile (normalise $ tail $ mediumPath medium)
                  runDB $ delete mediumId
                  setMessage "Medium succesfully deleted"
                  redirect $ HomeR
                _ -> do
                  setMessage "You must confirm the deletion"
                  redirect $ MediumSettingsR mediumId
            False -> do
              setMessage "You must own this medium to delete it"
              redirect $ MediumR mediumId
        Nothing -> do
          setMessage "You must be logged in to delete media"
          redirect $ LoginR
    Nothing -> do
      setMessage "This Medium does not exist"
      redirect $ HomeR
