import Database.HDBC
import Database.HDBC.PostgreSQL
import System.IO
import Control.Exception
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import System.FilePath as FP
import Filesystem.Path.CurrentOS
import Codec.Picture
import Codec.Picture.ScaleDCT
import qualified Codec.Picture.Metadata as PM

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
  let dbString = "host=" ++ dbHost ++ " port=" ++ dbPort ++ " user=" ++ dbUser ++ " dbname=" ++ dbName ++ " password=" ++ dbPasswd
  conn <- connectPostgreSQL dbString
  -- comment next two lines, if eidolon has been run after the update
  -- _ <- run conn "alter table medium add column preview varchar not null default 'fill_me!'" []
  -- _ <- run conn "alter table medium add column preview_width int8 not null default 0" []
  stmt1 <- prepare conn "select * from medium"
  _ <- execute stmt1 []
  rows <- fetchAllRowsAL stmt1
  -- mapM_ (putStrLn . show) rows
  tups <- mapM (\entry ->
    case entry of
      [("id", theId), _, ("path", SqlByteString path), _, _, _, ("owner", SqlInteger owner), _, _, _, _, ("album", SqlInteger album), _, _] -> do
        eimg <- readImageWithMetadata (tail $ B.unpack path)
        case eimg of
          Left e -> error e
          Right (orig, meta) -> do
            let newName = takeBaseName (B.unpack path) ++ "_preview.jpg"
                newPath = "static" FP.</> "data" FP.</> show owner FP.</> show album FP.</> newName
                origPix = convertRGBA8 orig
                oWidth  = fromIntegral $ fromJust $ PM.lookup PM.Width meta :: Int
                oHeight = fromIntegral $ fromJust $ PM.lookup PM.Height meta :: Int
                pHeight = 600
                pScale  = (fromIntegral pHeight :: Double) / (fromIntegral oHeight :: Double)
                pWidth  = floor (fromIntegral oWidth * pScale)
                pPix    = scale (pWidth, pHeight) origPix
            saveJpgImage 95 newPath $ ImageRGBA8 pPix
            return [SqlByteString (B.pack $ '/':newPath), SqlInteger (fromIntegral pWidth), theId]
      _ ->
        error "malformed entry"
    ) rows
  stmt2 <- prepare conn "update medium set preview = ?, preview_width = ? where id = ?"
  executeMany stmt2 tups
  commit conn
  disconnect conn
  putStrLn "Migration successfull!!"

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
