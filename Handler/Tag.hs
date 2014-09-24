module Handler.Tag where

import Import
import qualified Data.Text as T
import Data.Maybe

getTagR :: Text -> Handler Html
getTagR tag = do
  tempMedia <- runDB $ selectList [] [Asc MediumTitle]
  almostMedia <- mapM (\a -> do
    case tag `elem` (mediumTags $ entityVal a) of
      True -> return (Just a)
      False -> return Nothing
      ) tempMedia
  media <- return $ removeItem Nothing almostMedia
  defaultLayout $ do
    setTitle $ toHtml ("Eidolon :: Tag " `T.append` tag)
    $(widgetFile "tagMedia")
