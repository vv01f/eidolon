module Helper 
  ( getUserIdFromText
  , extractKey
--  , getUserNameById
  , fromHex
  , fromHex'
  , toHex
  , makeRandomToken
  , generateSalt
  )
where

import Prelude
import Model
import Control.Applicative
import Control.Monad.Trans.Class
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Types
import System.FilePath
import System.Random
import Yesod.Persist.Core
import Yesod.Core.Types
import Numeric (readHex, showHex)

getUserIdFromText :: T.Text -> UserId
getUserIdFromText tempUserId =
  Key $ PersistInt64 $ fromIntegral $ read $ T.unpack tempUserId

extractKey :: KeyBackend backend entity -> T.Text
extractKey = extractKey' . unKey
  where
    extractKey' (PersistInt64 k) = T.pack $ show k
    extractKey' _ = ""

--getUserNameById :: UserId -> Text
--getUserNameById userId =
--  let
--    user = runDB $ getJust $ userId
--  in
--    userName user

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
