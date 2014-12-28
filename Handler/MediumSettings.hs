module Handler.MediumSettings where

import Import
import Handler.Commons
import System.Directory
import System.FilePath
import Data.List (tail)

getMediumSettingsR :: MediumId -> Handler Html
getMediumSettingsR mediumId = do
  checkRes <- mediumCheck mediumId
  case checkRes of
    Right medium -> do
      (mediumSettingsWidget, enctype) <- generateFormPost $ mediumSettingsForm medium
      defaultLayout $ do
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
  <*> areq textareaField "Description" (Just $ mediumDescription medium)
  <*> areq tagField "tags" (Just $ mediumTags medium)
  <*> pure (mediumAlbum medium)

getMediumDeleteR :: MediumId -> Handler Html
getMediumDeleteR mediumId = do
  checkRes <- mediumCheck mediumId
  case checkRes of
    Right medium -> do
      defaultLayout $ do
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
