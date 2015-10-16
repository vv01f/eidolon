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

module Helper where

import Prelude
import Yesod.Static
import Model
import Data.Maybe
import Data.List as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Time
import Data.Char
import Database.Persist
import System.Random
-- import System.Locale
import Yesod
import Numeric (readHex, showHex)
import Network.Mail.Mime
import Text.Blaze.Html.Renderer.Utf8
import Graphics.ImageMagick.MagickWand
import Filesystem.Path.CurrentOS
import Database.Bloodhound
import Network.HTTP.Client
import Network.HTTP.Types.Status as S
import Control.Monad (when)

getUserIdFromText :: T.Text -> UserId
getUserIdFromText tempUserId =
  case key of
    Left a ->
      error $ T.unpack a
    Right k ->
      k
  where
    key = keyFromValues $ pInt64 : []
    pInt64 = PersistInt64 $ fromIntegral (read $ T.unpack tempUserId :: Integer)

extractKey :: PersistEntity record => Key record -> T.Text
extractKey = extractKey' . keyToValues
  where
    extractKey' [PersistInt64 k] = T.pack $ show k
    extractKey' _ = ""

packKey :: PersistEntity record => T.Text -> Key record
packKey = keyFromValues' . readText
  where
    readText t = PersistInt64 $ (fromIntegral $ read $ T.unpack t)
    keyFromValues' v = case keyFromValues [v] of
      Left err -> error $ T.unpack err
      Right k -> k

fromHex :: String -> BL.ByteString
fromHex = BL.pack . hexToWords
  where hexToWords (c:c':text) =
          let hex = [c, c']
              (word, _):_ = readHex hex
          in word : hexToWords text
        hexToWords _ = []

-- strict variant

fromHex' :: String -> B.ByteString
fromHex' = B.concat . BL.toChunks . fromHex

toHex :: B.ByteString -> T.Text
toHex = T.pack . concatMap mapByte . B.unpack
  where mapByte = pad 2 '0' . flip showHex ""
        pad len padding s
          | length s < len = pad len padding $ padding:s
          | otherwise = s

makeRandomToken :: IO T.Text
makeRandomToken = (T.pack . take 16 . randoms) `fmap` newStdGen

generateSalt :: IO B.ByteString
generateSalt = (B.pack . take 8 . randoms) <$> getStdGen

tagField :: Monad m => Field m [T.Text]
tagField = Field
  { fieldParse = \rawVals _ -> do
      case rawVals of
        [x] -> case L.null [x] of
          False -> return $ Right $ Just $ removeItem "" $ T.splitOn " " x
          True -> return $ Right $ Nothing
        _   -> return $ Left  $ error "unexpected tag list"
  , fieldView = \idAttr nameAttr _ eResult _ ->
      [whamlet|<input id=#{idAttr} type="text" name=#{nameAttr} value=#{either id (T.intercalate " ") eResult}>|]
  , fieldEnctype = UrlEncoded
  }

userField :: Monad m => [(T.Text, UserId)] -> Field m [UserId]
userField users = Field
  { fieldParse = \rawVals _ -> do
      case rawVals of
        [x] -> case x == "" of
          False ->
            -- clean = removeItem "" $ T.splitOn " " x
            let ids = map (\u -> lookup u users) (removeItem "" $ T.splitOn " " x)
            in case Nothing `elem` ids of
                False -> return $ Right $ Just $ nub $ map fromJust ids
                True -> return $ Left $ error "Invalid username list"
          True -> return $ Right $ Just $ []
        _ -> return $ Left $ error "unexpected username list"
  , fieldView = \idAttr nameAttr _ eResult _ ->
      [whamlet|<input id=#{idAttr} type="text" name=#{nameAttr} value=#{either id (getUsersFromResult users) eResult}>|]
  , fieldEnctype = UrlEncoded
  }

getUsersFromResult :: Eq b => [(T.Text, b)] -> [b] -> T.Text
getUsersFromResult users res = T.intercalate " " $ map (\x -> fromMaybe "" $ reverseLookup x users) res

sendMail :: MonadIO m => T.Text -> T.Text -> Html -> m ()
sendMail toEmail subject body =
  liftIO $ renderSendMail
    Mail
      { mailFrom = Address Nothing "noreply" -- TODO: set sender Address
      , mailTo = [Address Nothing toEmail]
      , mailCc = []
      , mailBcc = []
      , mailHeaders = [("Subject", subject)]
      , mailParts = [[Part
        { partType = "text/html; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partHeaders = []
        , partContent = renderHtml body
        }]]
      }

generateString :: IO T.Text
generateString = (toHex . B.pack . take 16 . randoms) <$> newStdGen

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys)
  | x == y    = removeItem x ys
  | otherwise = y : removeItem x ys

reverseLookup :: Eq b => b -> [(a, b)] -> Maybe a
reverseLookup s ((x, y):zs)
  | s == y    = Just x
  | s /= y    = reverseLookup s zs
  | otherwise = Nothing

acceptedTypes :: [T.Text]
acceptedTypes = ["image/jpeg", "image/jpg", "image/png", "image/x-ms-bmp", "image/x-bmp", "image/bmp", "image/tiff", "image/tiff-fx", "image/svg+xml", "image/gif"]

iso8601 :: FormatTime t => t -> String
iso8601 time =
  formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") time ++
  zone
  where zone = case formatTime defaultTimeLocale "%z" time of
                 (sig:digits@(h1:h2:m1:m2))
                   | sig `elem` ("+-" :: String) &&
                     all isDigit digits ->
                       sig:h1:h2:':':m1:m2
                 _ ->
                   "Z"

localTimeToZonedTime :: TimeZone -> LocalTime -> ZonedTime
localTimeToZonedTime tz =
  utcToZonedTime tz . localTimeToUTC tz

rfc822 :: FormatTime t => t -> String
rfc822 = formatTime defaultTimeLocale rfc822DateFormat

mediumStaticImageRoute :: Medium -> Route Static
mediumStaticImageRoute medium =
  StaticRoute (drop 2 $ T.splitOn "/" $ T.pack $ mediumPath medium) []

mediumStaticThumbRoute :: Medium -> Route Static
mediumStaticThumbRoute medium =
  StaticRoute (drop 2 $ T.splitOn "/" $ T.pack $ mediumThumb medium) []

getThumbWidth :: MonadIO m => Maybe String -> m Int
getThumbWidth path
  | path == Nothing = pure 230
  | otherwise       = liftIO $ withMagickWandGenesis $ do
                        (_, w) <- magickWand
                        readImage w (decodeString $ fromJust path)
                        getImageWidth w

multiFileField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m [FileInfo]
multiFileField = Field
    { fieldParse = \_ files -> return $
        case files of
            [] -> Right Nothing
            file:_ -> Right $ Just files
    , fieldView = \id' name attrs _ isReq -> toWidget [hamlet|
            <input id=#{id'} name=#{name} *{attrs} type=file :isReq:required multiple="" enctype="multipart/form-data">
        |]
    , fieldEnctype = Multipart
    }

-- <putIndexES :: ESInput -> Handler ()
putIndexES input = do
  resp <- case input of
    ESUser uId user -> do
      ex <- runBH' $ indexExists (IndexName "user")
      when (not ex) ((\ _ -> do
        runBH' $ createIndex defaultIndexSettings (IndexName "user")
        return ()
        ) ex)
      _ <- runBH' $ openIndex (IndexName "user")
      runBH' $ indexDocument (IndexName "user") (MappingName "object") defaultIndexDocumentSettings user (DocId $ extractKey uId)
    ESAlbum aId album -> do
      ex <- runBH' $ indexExists (IndexName "album")
      when (not ex) ((\ _ -> do
        runBH' $ createIndex defaultIndexSettings (IndexName "album")
        return ()
        ) ex)
      _ <- runBH' $ openIndex (IndexName "album")
      runBH' $ indexDocument (IndexName "album") (MappingName "object") defaultIndexDocumentSettings album (DocId $ extractKey aId)
    ESMedium mId medium -> do
      ex <- runBH' $ indexExists (IndexName "medium")
      when (not ex) ((\ _ -> do
        runBH' $ createIndex defaultIndexSettings (IndexName "medium")
        return ()
        ) ex)
      _ <- runBH' $ openIndex (IndexName "medium")
      runBH' $ indexDocument (IndexName "medium") (MappingName "object") defaultIndexDocumentSettings medium (DocId $ extractKey mId)
    ESComment cId comment -> do
      ex <- runBH' $ indexExists (IndexName "comment")
      when (not ex) ((\ _ -> do
        runBH' $ createIndex defaultIndexSettings (IndexName "comment")
        return ()
        ) ex)
      _ <- runBH' $ openIndex (IndexName "comment")
      runBH' $ indexDocument (IndexName "comment") (MappingName "object") defaultIndexDocumentSettings comment (DocId $ extractKey cId)
  case statusCode (responseStatus resp) of
    201 -> return ()
    -- 200 -> return ()
    _ -> error $ C.unpack $ BL.toStrict $ responseBody resp

-- deleteIndexES :: ESInput -> Handler ()
deleteIndexES input = do
  resp <- case input of
    ESUser uId user ->
      runBH' $ deleteDocument (IndexName "user") (MappingName "object") (DocId $ extractKey uId)
    ESAlbum aId album ->
      runBH' $ deleteDocument (IndexName "album") (MappingName "object") (DocId $ extractKey aId)
    ESMedium mId medium ->
      runBH' $ deleteDocument (IndexName "medium") (MappingName "object") (DocId $ extractKey mId)
    ESComment cId comment ->
      runBH' $ deleteDocument (IndexName "comment") (MappingName "object") (DocId $ extractKey cId)
  case statusCode (responseStatus resp) of
    201 -> return ()
    -- 200 -> return ()
    _ -> error $ C.unpack $ BL.toStrict $ responseBody resp 

runBH' action = do
  let server = Server "http://localhost:9200"
  manager <- newManager defaultManagerSettings
  runBH (BHEnv server manager) action
