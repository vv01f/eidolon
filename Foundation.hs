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

{-# LANGUAGE FlexibleInstances #-}

module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import Database.Persist.Sql -- (ConnectionPool, runSqlPool)
import Settings.StaticFiles
import Settings
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Yesod.Core.Types
-- costom imports
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Maybe (fromJust)
import Network.Wai
import Helper hiding (hmacKeccak)
import Yesod.Auth
import Yesod.Auth.HmacKeccak
import System.IO.Unsafe (unsafePerformIO)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

renderLayout :: Widget -> Handler Html
renderLayout widget = do
    master <- getYesod
    route <- getCurrentRoute
    mmsg <- getMessage
    -- msu <- lookupSession "userId"
    musername <- maybeAuthId
    slug <- case musername of
      Just name -> do
        user <- runDB $ getBy $ UniqueUser name
        return $ userSlug $ entityVal $ fromJust user
      Nothing ->
        return ("" :: T.Text)
    admin <- case musername of
      Just name -> do
        user <- runDB $ getBy $ UniqueUser name
        return $ userAdmin $ entityVal $ fromJust user
      Nothing ->
        return False
    let block = appSignupBlocked $ appSettings master

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    -- searchWidget <- widgetToPageContent $ [whamlet|
    --   <form action=@{SearchR} method=GET>
    --     <input type="hidden" name="_hasdata">
    --     <div .input-group .required>
    --       <input #hident2 .form-control type="text" autofocus="" required="" name="f1" placeholder="Search for ...">
    --       <span .input-group-btn>
    --         <button .btn .btn-default type="submit">Go!
    --     <script>
    --       if (!('autofocus' in document.createElement('input'))) {document.getElementById('hident2').focus();}
    --   |]

    wc <- widgetToPageContent widget

    pc <- widgetToPageContent $ do
        mapM_ addScript $ map StaticR
            [ js_picturefill_js
            , js_jquery_min_js
            ]
        mapM_ addStylesheet $ map StaticR
            [ css_bootstrap_min_css
            , css_dropdown_css
            , css_main_css
            ]
        $(widgetFile "default-layout")

    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

approotRequest :: App -> Request -> T.Text
approotRequest master req =
    case requestHeaderHost req of
      Just a  -> prefix `T.append` decodeUtf8 a
      Nothing -> appRoot $ appSettings master
    where
      prefix =
        if
          "https://" `T.isPrefixOf` appRoot (appSettings master)
          then
            "https://"
          else
            "http://"

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    --approot = ApprootMaster $ appRoot . appSettings
    approot = ApprootRequest approotRequest

    -- change maximum content length
    maximumContentLength _ _ = Just $ 1024 ^ (5 :: Int)

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- defaultLayout widget =
    --   renderLayout $(widgetFile "default-widget")
    defaultLayout = renderLayout

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    -- urlRenderOverride y (StaticR s) =
    --     Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    -- urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    isAuthorized AdminR                    _ = getAdminAuth
    isAuthorized AdminProfilesR            _ = getAdminAuth
    isAuthorized (AdminProfileSettingsR _) _ = getAdminAuth
    isAuthorized (AdminUserAlbumsR _)      _ = getAdminAuth
    isAuthorized (AdminUserMediaR _)       _ = getAdminAuth
    isAuthorized (AdminProfileDeleteR _)   _ = getAdminAuth
    isAuthorized AdminAlbumsR              _ = getAdminAuth
    isAuthorized (AdminAlbumSettingsR _)   _ = getAdminAuth
    isAuthorized (AdminAlbumMediaR _)      _ = getAdminAuth
    isAuthorized (AdminAlbumDeleteR _)     _ = getAdminAuth
    isAuthorized AdminMediaR               _ = getAdminAuth
    isAuthorized (AdminMediumSettingsR _)  _ = getAdminAuth
    isAuthorized (AdminMediumDeleteR _)    _ = getAdminAuth
    isAuthorized AdminCommentR             _ = getAdminAuth
    isAuthorized (AdminCommentDeleteR _)   _ = getAdminAuth
    isAuthorized _                         _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- Place Javascript at head so scripts become loaded before content
    jsLoader _ = BottomOfHeadBlocking

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

getAdminAuth = do
  musername <- maybeAuthId
  case musername of
    Nothing -> return AuthenticationRequired
    Just un -> do
      muser <- runDB $ getBy $ UniqueUser un
      return $ case muser of
        Just (Entity _ u)
          | isAdmin u -> Authorized
          | otherwise -> Unauthorized "You are not authorized"
        Nothing -> AuthenticationRequired

isAdmin = userAdmin

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = Username

  -- Where to send a user after successful login
    loginDest _ = HomeR
  -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId = return . Just . credsIdent

  -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [hmacPlugin]

    authHttpManager = error "no HttpManager needed"

    maybeAuthId = lookupSession credsKey

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
-- getExtra :: Handler Extra
-- getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

instance YesodHmacKeccak (HmacPersistDB App User Token) App where
  runHmacDB = runHmacPersistDB
  rawLoginRoute = Just LoginRawR

instance UserCredentials (Entity User) where
  userUserName = userName . entityVal
  userUserSalt = userSalt . entityVal
  userUserSalted = userSalted . entityVal
  userUserEmail = userEmail . entityVal
  userUserActive = userActive . entityVal

instance TokenData (Entity Token) where
  tokenTokenKind = tokenKind . entityVal
  tokenTokenUsername = tokenUsername . entityVal
  tokenTokenToken = tokenToken . entityVal

instance PersistUserCredentials User where
  userUsernameF   = UserName
  userUserSaltF   = UserSalt
  userUserSaltedF = UserSalted
  userUserEmailF  = UserEmail
  userUserActiveF = UserActive
  uniqueUsername  = UniqueUser

  userCreate name email salt = User name name email salt "" [] False (-1) False

instance PersistToken Token where
  tokenTokenTokenF = TokenToken
  tokenTokenKindF = TokenKind
  tokenTokenUsernameF = TokenUsername
  uniqueToken = UniqueToken

  tokenCreate t u k = Token t k u

instance HmacSendMail App where
  sendVerifyEmail username email url =
    sendMail email "Please activate your account!" $
      [shamlet|
<h1>Hello #{username} and welcome to Eidolon!
To complete your signup process, please activate your account by visiting the
following link:
<a href=#{url}>#{url}

See you soon!
      |]

  sendReactivateEmail username email url = do
    muser <- runDB $ getBy $ UniqueUser username
    let user = entityVal $ fromJust muser
    sendMail email "Reset your password" $
      [shamlet|
<h1>Welcome again to Eidolon #{userSlug user}
To reset your password visit the following link:
<a href=#{url}>#{url}

See you soon!
      |]
