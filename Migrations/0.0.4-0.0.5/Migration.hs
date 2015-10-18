{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Migrate2 where

import Prelude
import Yesod.Core (liftIO)
import Database.HDBC
import Database.HDBC.PostgreSQL
import System.IO
import Control.Exception
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson as A
import Data.Time.Clock
import Data.Yaml as Y
import Data.Time.LocalTime
import Database.Bloodhound
import Network.HTTP.Client
import Network.HTTP.Types.Status

data ESSettings = ESSettings
  { esHost :: T.Text
  , esShards :: Int
  , esReplicas :: Int
  }

instance A.FromJSON ESSettings where
  parseJSON = withObject "AppSettings" $ \o -> do
    esHost <- o .: "searchhost"
    esShards <- o .: "shards"
    esReplicas <- o .: "replicas"
    return ESSettings {..}

main :: IO ()
main = do
  putStrLn "Enter database host"
  dbHost <- getLine
  putStrLn "Enter database port"
  dbPort <- getLine
  putStrLn "Enter database user"
  dbUser <- getLine
  putStrLn "Enter database name"
  dbName <- getLine
  putStrLn "Enter database password"
  dbPasswd <- getPasswd
  putStrLn "Enter location of eidolon settings.yml file"
  settingsFP <- getLine
  let dbString = "host=" ++ dbHost ++ " port=" ++ dbPort ++ " user=" ++ dbUser ++ " dbname=" ++ dbName ++ " password=" ++ dbPasswd
  conn <- connectPostgreSQL dbString
  settingsY <- decodeFile settingsFP :: IO (Maybe ESSettings)
  elastic <- case settingsY of
    Just el -> return el
    Nothing -> error $ "Could not read settings from " ++ settingsFP
  let server = Server $ esHost elastic
  let indexSettings = IndexSettings (ShardCount $ esShards elastic) (ReplicaCount $ esReplicas elastic)
  userStmt <- prepare conn "select * from \"user\""
  _ <- execute userStmt []
  userRows <- fetchAllRowsAL userStmt
  albumStmt <- prepare conn "select * from \"album\""
  _ <- execute albumStmt []
  albumRows <- fetchAllRowsAL albumStmt
  mediumStmt <- prepare conn "select * from \"medium\""
  _ <- execute mediumStmt []
  mediumRows <- fetchAllRowsAL mediumStmt
  commentStmt <- prepare conn "select * from \"comment\""
  _ <- execute commentStmt []
  commentRows <- fetchAllRowsAL commentStmt
  _ <- withBH defaultManagerSettings server $ do
    _ <- createIndex indexSettings (IndexName "user")
    _ <- createIndex indexSettings (IndexName "album")
    _ <- createIndex indexSettings (IndexName "medium")
    _ <- createIndex indexSettings (IndexName "comment")
    _ <- sequence $ map (\entry ->
      case entry of
        [("id", SqlInteger theId), ("name", SqlByteString name), ("slug", SqlByteString slug), _, _, _, _, _] -> do
           let u = SUser (decodeUtf8 name) (decodeUtf8 slug)
           let dId = DocId $ T.pack $ show theId
           liftIO $ putStrLn $ (show u) ++ "\n"
           resp <- indexDocument (IndexName "user") (MappingName "user") defaultIndexDocumentSettings u dId
           case statusCode (responseStatus resp) of
             200 -> liftIO $ putStrLn $ "OK: " ++ (BL.unpack $ responseBody resp)
             201 -> liftIO $ putStrLn $ "OK: " ++ (BL.unpack $ responseBody resp)
             _   -> liftIO $ putStrLn $ "!!Failure!!: " ++ (BL.unpack $ responseBody resp)
        bla ->
          error $ "malformed entry" ++ show bla
      ) userRows
    _ <- sequence $ map (\entry ->
      case entry of
        [("id", SqlInteger theId), ("title", SqlByteString title), _, _, _, _, _] -> do
           let a = SAlbum (decodeUtf8 title)
           let dId = DocId $ T.pack $ show theId
           liftIO $ putStrLn $ (show a) ++ "\n"
           resp <- indexDocument (IndexName "album") (MappingName "album") defaultIndexDocumentSettings a dId
           case statusCode (responseStatus resp) of
             200 -> liftIO $ putStrLn $ "OK: " ++ (BL.unpack $ responseBody resp)
             201 -> liftIO $ putStrLn $ "OK: " ++ (BL.unpack $ responseBody resp)
             _   -> liftIO $ putStrLn $ "!!Failure!!: " ++ (BL.unpack $ responseBody resp)
        bla ->
          error $ "malformed entry: " ++ show bla
      ) albumRows
    _ <- sequence $ map (\entry ->
      case entry of
        [("id", SqlInteger theId), ("title", SqlByteString title), _, _, _, ("time", SqlZonedTime time), _, ("description", SqlByteString desc), ("tags", SqlByteString tags), _, _, _, _, _] -> do
           let m = SMedium (decodeUtf8 title) (zonedTimeToUTC time) (decodeUtf8 desc) (parseTags tags)
           let dId = DocId $ T.pack $ show theId
           liftIO $ putStrLn $ (show m) ++ "\n"
           resp <- indexDocument (IndexName "medium") (MappingName "medium") defaultIndexDocumentSettings m dId
           case statusCode (responseStatus resp) of
             200 -> liftIO $ putStrLn $ "OK: " ++ (BL.unpack $ responseBody resp)
             201 -> liftIO $ putStrLn $ "OK: " ++ (BL.unpack $ responseBody resp)
             _   -> liftIO $ putStrLn $ "!!Failure!!: " ++ (BL.unpack $ responseBody resp)
        bla ->
          error $ "malformed entry" ++ show bla
      ) mediumRows
    _ <- sequence $ map (\entry ->
      case entry of
        [("id", SqlInteger theId), _, ("author_slug", SqlByteString author), _, _, ("time", SqlZonedTime time), ("content", SqlByteString content)] -> do
           let c = SComment (decodeUtf8 author) (zonedTimeToUTC time) (decodeUtf8 content)
           let dId = DocId $ T.pack $ show theId
           liftIO $ putStrLn $ (show c) ++ "\n"
           resp <- indexDocument (IndexName "medium") (MappingName "medium") defaultIndexDocumentSettings c dId
           case statusCode (responseStatus resp) of
             200 -> liftIO $ putStrLn $ "OK: " ++ (BL.unpack $ responseBody resp)
             201 -> liftIO $ putStrLn $ "OK: " ++ (BL.unpack $ responseBody resp)
             _   -> liftIO $ putStrLn $ "!!Failure!!: " ++ (BL.unpack $ responseBody resp)
        bla ->
          error $ "malformed entry" ++ show bla
      ) commentRows
    return ()
  putStrLn "Migration successful!!"

data SUser = SUser
  { suName :: T.Text
  , suSlug :: T.Text
  } deriving Show

instance A.ToJSON SUser where
  toJSON (SUser n s) = object
    [ "name" .= n
    , "slug" .= s
    ]

data SAlbum = SAlbum
  { saName :: T.Text } deriving Show

instance A.ToJSON SAlbum where
  toJSON (SAlbum n) = object
    [ "name" .= n ]

data SMedium = SMedium
  { smName :: T.Text
  , smTime :: UTCTime
  , smDesc :: T.Text
  , smTags :: [T.Text]
  } deriving Show

instance A.ToJSON SMedium where
  toJSON (SMedium n t d g) = object
    [ "name"        .= n
    , "time"        .= t
    , "description" .= d
    , "tags"        .= g
    ]

data SComment = SComment
  { scAuthor :: T.Text
  , scTime :: UTCTime
  , scContent :: T.Text
  } deriving Show

instance A.ToJSON SComment where
  toJSON (SComment a t c) = object
    [ "author"  .= a
    , "time"    .= t
    , "content" .= c
    ]

parseTags :: B.ByteString -> [T.Text]
parseTags bs = map handle' inner
  where
    inner = T.splitOn "," $ T.pack $ B.unpack $ B.init $ B.tail bs
    handle' = T.dropEnd 1 . T.drop 2

getPasswd :: IO String
getPasswd = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
