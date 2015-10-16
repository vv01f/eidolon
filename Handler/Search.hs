module Handler.Search where

import Import
import Helper
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import Database.Bloodhound
import Database.Bloodhound.Client
import Network.HTTP.Client (defaultManagerSettings, responseBody)

getSearchR :: Handler Html
getSearchR = do
  ((res, widget), _) <- runFormGet searchForm
  searchResults <-
    case res of
      FormSuccess query -> do
        res <- getResults query
        return $ L.toStrict $ responseBody res
      _ -> return $ C.empty
  -- error $ C.unpack searchResults
  defaultLayout $
    $(widgetFile "search")

searchForm :: Form Text
searchForm = renderDivs $ areq (searchField True) "Search" Nothing

getResults query = do
  let esQuery = QuerySimpleQueryStringQuery (SimpleQueryStringQuery (QueryString query) Nothing Nothing Nothing Nothing Nothing Nothing)
  liftIO $ runBH' $ searchAll $ mkSearch (Just esQuery) Nothing
