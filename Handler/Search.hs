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

module Handler.Search where

import Import
import Handler.Commons
import Data.Time.Clock
import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import Database.Bloodhound
import Network.HTTP.Client (responseBody)
import System.FilePath.Posix

import Debug.Trace

getSearchR :: Handler Html
getSearchR = do
  ((res, widget), _) <- runFormGet searchForm
  case res of
    FormSuccess query -> do
      (ru, ra, rm, rc) <- getResults query
      liftIO $ traceIO $ show (ru, ra, rm, rc)
      let a = decode (responseBody ru) :: Maybe (SearchResult SearchUser)
      let b = decode (responseBody ra) :: Maybe (SearchResult SearchAlbum)
      let c = decode (responseBody rm) :: Maybe (SearchResult SearchMedium)
      let d = decode (responseBody rc) :: Maybe (SearchResult SearchComment)
      liftIO $ traceIO $ show (a,b,c,d)
      let hitListA = case a of {
          Just as -> hits $ searchHits as;
          Nothing -> []}
      let hitListB = case b of {
          Just bs -> hits $ searchHits bs;
          Nothing -> []}
      let hitListC = case c of {
          Just cs -> hits $ searchHits cs;
          Nothing -> []}
      let hitListD = case d of {
          Just ds -> hits $ searchHits ds;
          Nothing -> []}
      userIdList <- return $ mapMaybe (\h -> do
        if
          hitIndex h == IndexName "user"
          then do
            DocId theId <- return $ hitDocId h
            Just $ (packKey theId :: UserId)
          else
            Nothing
        ) hitListA
      albumIdList <- return $ mapMaybe (\h -> do
        if
          hitIndex h == IndexName "album"
          then do
            DocId theId <- return $ hitDocId h
            Just $ (packKey theId :: AlbumId)
          else
            Nothing
        ) hitListB
      mediumIdList <- return $ mapMaybe (\h -> do
        if
          hitIndex h == IndexName "medium"
          then do
            DocId theId <- return $ hitDocId h
            Just $ (packKey theId :: MediumId)
          else
            Nothing
        ) hitListC
      commentIdList <- return $ mapMaybe (\h -> do
        if
          hitIndex h == IndexName "comment"
          then do
            DocId theId <- return $ hitDocId h
            Just $ (packKey theId :: CommentId)
          else
            Nothing
        ) hitListD
      liftIO $ traceIO $ show (userIdList, albumIdList, mediumIdList, commentIdList)
      userList <- return . catMaybes =<< mapM (\i -> runDB $ selectFirst [UserId ==. i] []) userIdList
      albumList <- return . catMaybes =<< mapM (\i -> runDB $ selectFirst [AlbumId ==. i] []) albumIdList
      mediumList <- return . catMaybes =<< mapM (\i -> runDB $ selectFirst [MediumId ==. i] []) mediumIdList
      commentList <- return . catMaybes =<< mapM (\i -> runDB $ selectFirst [CommentId ==. i] []) commentIdList
      let allEmpty = (null userList) && (null albumList) && (null mediumList) && (null commentList)
      defaultLayout $ do
        setTitle $ toHtml $ "Eidolon :: Search results for " ++ (T.unpack query)
        $(widgetFile "result")
    _ ->
      defaultLayout $ do
        setTitle "Eidolon :: Search"
        $(widgetFile "search")

searchForm :: Form T.Text
searchForm = renderDivs $ areq (searchField True) "Search" Nothing

getResults :: Text -> Handler (Reply, Reply, Reply, Reply)
getResults query = do
  -- esQuery <- return $ QueryFuzzyLikeThisQuery $ FuzzyLikeThisQuery
  --   { fuzzyLikeFields              = [FieldName "_all"]
  --   , fuzzyLikeText                = query
  --   , fuzzyLikeMaxQueryTerms       = MaxQueryTerms 25
  --   , fuzzyLikeIgnoreTermFrequency = IgnoreTermFrequency False
  --   , fuzzyLikeFuzziness           = Fuzziness 0.6
  --   , fuzzyLikePrefixLength        = PrefixLength 0
  --   , fuzzyLikeBoost               = Boost 1.0
  --   , fuzzyLikeAnalyzer            = Nothing
  --   }
  esQuery <- return $ QueryFuzzyQuery $ FuzzyQuery
    { fuzzyQueryField         = FieldName "_all"
    , fuzzyQueryValue         = query
    , fuzzyQueryPrefixLength  = PrefixLength 0
    , fuzzyQueryMaxExpansions = MaxExpansions 50
    , fuzzyQueryFuzziness     = Fuzziness 0.6
    , fuzzyQueryBoost         = Just (Boost 1.0)
    }
  su <- runBH' $ searchByIndex (IndexName "user") $ mkSearch (Just esQuery) Nothing
  sa <- runBH' $ searchByIndex (IndexName "album") $ mkSearch (Just esQuery) Nothing
  sm <- runBH' $ searchByIndex (IndexName "medium") $ mkSearch (Just esQuery) Nothing
  sc <- runBH' $ searchByIndex (IndexName "comment") $ mkSearch (Just esQuery) Nothing
  return (su, sa, sm, sc)

data SearchUser = SearchUser
  { suName :: T.Text
  , suSlug :: T.Text
  } deriving Show

instance FromJSON SearchUser where
  parseJSON (Object o) = SearchUser
    <$> o .: "name"
    <*> o .: "slug"
  parseJSON _ = mempty

data SearchAlbum = SearchAlbum
  { saName :: T.Text } deriving Show

instance FromJSON SearchAlbum where
  parseJSON (Object o) = SearchAlbum <$> o .: "name"
  parseJSON _ = mempty

data SearchMedium = SearchMedium
  { smName :: Text
  , smTime :: UTCTime
  , smDescription :: Textarea
  , smTags :: [T.Text]
  } deriving Show

instance FromJSON SearchMedium where
  parseJSON (Object o) = SearchMedium
    <$> o .: "name"
    <*> o .: "time"
    <*> o .: "description"
    <*> o .: "tags"
  parseJSON _ = mempty

data SearchComment = SearchComment
  { scAuthor :: Text
  , scTime :: UTCTime
  , scContent :: Text
  } deriving Show

instance FromJSON SearchComment where
  parseJSON (Object o) = SearchComment
    <$> o .: "author"
    <*> o .: "time"
    <*> o .: "content"
  parseJSON _ = mempty
