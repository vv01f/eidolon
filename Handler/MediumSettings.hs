module Handler.MediumSettings where

import Import

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
