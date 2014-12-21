module Handler.AdminMediumSettings where

import Import
import System.FilePath
import System.Directory
import Data.List (tail)

getAdminMediaR :: Handler Html
getAdminMediaR = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True -> do
          media <- runDB $ selectList [] [Asc MediumTitle]
          defaultLayout $ do
            setTitle "Administration: Media"
            $(widgetFile "adminMedia")
        False -> do
          setMessage "You are no admin"
          redirect $ HomeR
    Nothing -> do
      setMessage "You must be logged in"
      redirect $ LoginR

getAdminMediumSettingsR :: MediumId -> Handler Html
getAdminMediumSettingsR mediumId = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True -> do
          tempMedium <- runDB $ get mediumId
          case tempMedium of
            Just medium -> do
              (adminMediumSetWidget, enctype) <- generateFormPost $ adminMediumSetForm medium
              defaultLayout $ do
                setTitle "Administration: Medium Settings"
                $(widgetFile "adminMediumSet")
            Nothing -> do
              setMessage "This medium does not exist"
              redirect $ AdminR
        False -> do
          setMessage "You are no admin"
          redirect $ HomeR
    Nothing -> do
      setMessage "You must be logged in"
      redirect $ LoginR

postAdminMediumSettingsR :: MediumId -> Handler Html
postAdminMediumSettingsR mediumId = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True -> do
          tempMedium <- runDB $ get mediumId
          case tempMedium of
            Just medium -> do
              ((res, adminMediumSetWidget), enctype) <- runFormPost $ adminMediumSetForm medium
              case res of
                FormSuccess temp -> do
                  runDB $ update mediumId
                    [ MediumTitle =. mediumTitle temp
                    , MediumDescription =. mediumDescription temp
                    , MediumTags =. mediumTags temp
                    ]
                  setMessage "Medium settings changed successfully"
                  redirect $ AdminR
                _ -> do
                  setMessage "There was an error while changing the settings"
                  redirect $ AdminMediumSettingsR mediumId
            Nothing -> do
              setMessage "This medium does not exist"
              redirect $ AdminR
        False -> do
          setMessage "You are no admin"
          redirect $ HomeR
    Nothing -> do
      setMessage "You must be logged in"
      redirect $ LoginR

adminMediumSetForm :: Medium -> Form Medium
adminMediumSetForm medium = renderDivs $ Medium
  <$> areq textField "Title" (Just $ mediumTitle medium)
  <*> pure (mediumPath medium)
  <*> pure (mediumThumb medium)
  <*> pure (mediumMime medium)
  <*> pure (mediumTime medium)
  <*> pure (mediumOwner medium)
  <*> areq textareaField "Description" (Just $ mediumDescription medium)
  <*> areq tagField "Tags" (Just $ mediumTags medium)
  <*> pure (mediumAlbum medium)

getAdminMediumDeleteR :: MediumId -> Handler Html
getAdminMediumDeleteR mediumId = do
  msu <- lookupSession "userId"
  case msu of
    Just tempUserId -> do
      userId <- return $ getUserIdFromText tempUserId
      user <- runDB $ getJust userId
      case userAdmin user of
        True -> do
          tempMedium <- runDB $ get mediumId
          case tempMedium of
            Just medium -> do
              -- remove reference from album
              albumId <- return $ mediumAlbum medium
              album <- runDB $ getJust albumId
              mediaList <- return $ albumContent album
              newMediaList <- return $ removeItem mediumId mediaList
              runDB $ update albumId [AlbumContent =. newMediaList]
              -- delete comments
              commEnts <- runDB $ selectList [CommentOrigin ==. mediumId] []
              mapM (\ent -> runDB $ delete $ entityKey ent) commEnts
              -- delete medium
              runDB $ delete mediumId
              -- delete files
              liftIO $ removeFile (normalise $ tail $ mediumPath medium)
              liftIO $ removeFile (normalise $ tail $ mediumPath medium)
              -- outro
              setMessage "Medium deleted successfully"
              redirect $ AdminR
            Nothing -> do
              setMessage "This medium does not exist"
              redirect $ AdminR
        False -> do
          setMessage "You are no admin"
          redirect $ HomeR
    Nothing -> do
      setMessage "You must be logged in"
      redirect $ LoginR
