import Database.HDBC
import Database.HDBC.PostgreSQL
import System.IO
import System.Directory
import Control.Exception
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Data.List.Split
import System.FilePath as FP
import Filesystem.Path.CurrentOS
import Codec.Picture as P
import Codec.Picture.ScaleDCT
import qualified Codec.Picture.Metadata as PM
import Graphics.Svg as SVG
import Graphics.Rasterific.Svg
import Graphics.Text.TrueType
import Magic

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
      [("id", theId), _, ("path", SqlByteString bPath), _, _, _, ("owner", SqlInteger owner), _, _, ("album", SqlInteger album), _] -> do
        let path = tail $ B.unpack bPath
        eimg <- readImage path
        orig <- case eimg of
          Left err -> do -- This branch contains svg and other data formats. to be extended for pdf et al.
            magic <- magicOpen [MagicMime]
            magicLoadDefault magic
            putStrLn path
            mime <- head <$> splitOn ";" <$> magicFile magic path
            putStrLn mime
            case mime of
              "image/svg+xml" -> do
                svg <- loadSvgFile path
                (img, _) <- renderSvgDocument emptyFontCache Nothing 100 $ fromJust svg
                return img
              _ -> error err
          Right img -> do -- This branch contains "classical" image formats like bmp or png
            putStrLn path
            return $ convertRGBA8 img
        let thumbName = FP.takeBaseName path ++ "_thumb.png"
            prevName = FP.takeBaseName path ++ "_preview.png"
            oldThumbName = FP.takeBaseName path ++ "_thumb.jpg"
            oldPrevName = FP.takeBaseName path ++ "_preview.jpg"
            pathPrefix = "static" FP.</> "data" FP.</> show owner FP.</> show album
            tPath = pathPrefix FP.</> thumbName
            pPath = pathPrefix FP.</> prevName
            -- origPix = convertRGBA8 orig
            oWidth = P.imageWidth orig :: Int
            oHeight = P.imageHeight orig :: Int
            tWidth = floor (fromIntegral oWidth / fromIntegral oHeight * fromIntegral tHeight :: Double)
            tHeight = 230 :: Int
            pHeight = 600 :: Int
            pScale = (fromIntegral pHeight :: Double) / (fromIntegral oHeight :: Double)
            pWidth = floor (fromIntegral oWidth * pScale)
            tPix = scale (tWidth, tHeight) orig
            pPix = scale (pWidth, pHeight) orig
        savePngImage tPath $ ImageRGBA8 tPix
        savePngImage pPath $ ImageRGBA8 pPix
        mapM (removeFile . (FP.</>) pathPrefix) [oldThumbName, oldPrevName]
        return [SqlByteString (B.pack $ '/':tPath), SqlByteString (B.pack $ '/':pPath), theId]
      _ ->
        error "malformed entry"
    ) rows
  stmt2 <- prepare conn "update medium set thumb = ?, preview = ? where id = ?"
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
