{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import Database.HDBC
import Database.HDBC.PostgreSQL
import System.IO
import Control.Exception
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Network.Mail.Mime
import Text.Hamlet
import System.Random
import Text.Blaze.Html.Renderer.Utf8
import Numeric (showHex)

main :: IO ()
main = do
  putStrLn "Enter app root (e.g.: \"https://eidolon.nek0.eu\""
  appRoot <- getLine
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
  stmt1 <- prepare conn "select * from \"user\""
  _ <- execute stmt1 []
  rows <- fetchAllRowsAL stmt1
  mapM_ (\entry ->
    case entry of
      [("id", SqlInteger theId), _, ("slug", SqlByteString uSlug), ("email", SqlByteString uEmail), _, _, _, _] -> do
        rText <- (toHex . B.pack . take 16 . randoms) <$> newStdGen
        _ <- run conn ("insert into \"token\" (kind, token, \"user\") values (" ++
          "'activate', " ++
          "'" ++ BC.unpack rText ++ "', " ++
          show theId ++ ")") []
        let link = appRoot ++ "/activate/" ++ BC.unpack rText
        sendMail (BC.unpack uEmail) (BC.unpack uSlug) link
        putStrLn $ "Notified " ++ (BC.unpack uSlug)
      _ ->
        error "Malformed entry"
    ) rows
  commit conn
  disconnect conn
  putStrLn "Migration successfull!!"

sendMail :: String -> String -> String -> IO ()
sendMail toEmail userSlug link = do
  renderSendMail
    Mail
      { mailFrom = Address Nothing "noreply"
      , mailTo = [Address Nothing (T.pack toEmail)]
      , mailCc = []
      , mailBcc = []
      , mailHeaders = [("Subject", "Please update your password")]
      , mailParts = [[Part
        { partType = "text/html; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partHeaders = []
        , partContent = renderHtml [shamlet|
            <h1>Hello #{userSlug},
            <p>
              Due to changes in the Database your Password has been reset.
              Please reactivate your account by going to <a href="#{link}">#{link}
            |]
        }]]
      }

toHex :: B.ByteString -> B.ByteString
toHex = BC.pack . concatMap mapByte . B.unpack
  where mapByte = pad 2 '0' . flip showHex ""
        pad len padding s
          | length s < len = pad len padding $ padding:s
          | otherwise = s

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
