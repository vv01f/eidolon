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
  _ <- run conn "alter table album drop column sample_width" []
  _ <- run conn "alter table medium drop column width" []
  _ <- run conn "alter table medium drop column thumb_width" []
  _ <- run conn "alter table medium drop column preview_width" []
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
