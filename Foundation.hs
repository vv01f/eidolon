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
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.module Handler.Activate where

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
import Data.Text
import Helper

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
    mmsg <- getMessage
    msu <- lookupSession "userId"
    username <- case msu of
      Just a -> do
        uId <- return $ getUserIdFromText a
        user <- runDB $ getJust uId
        return $ userName user
      Nothing -> do
        return ("" :: Text)
    slug <- case msu of
      Just a -> do
        uId <- return $ getUserIdFromText a
        user <- runDB $ getJust uId
        return $ userSlug user
      Nothing -> do
        return ("" :: Text)
    block <- return $ appSignupBlocked $ appSettings master

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    wc <- widgetToPageContent widget

    pc <- widgetToPageContent $ do
        -- add parallelism js files
        $(combineScripts 'StaticR
            [ js_jquery_min_js
            , js_jquery_poptrox_min_js
            , js_skel_min_js
            , js_init_js
            ])
        $(combineStylesheets 'StaticR
            [
            --  css_normalize_css
              css_bootstrap_css
            ])
        $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

formLayout :: Widget -> Handler Html
formLayout widget = do
    renderLayout $(widgetFile "form-widget")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . appSettings

    -- change maximum content length
    maximumContentLength _ _ = Just $ 1024 ^ (5 :: Int)

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
      renderLayout $(widgetFile "default-widget")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    -- urlRenderOverride y (StaticR s) =
    --     Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    -- urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    -- authRoute _ = Just $ AuthR LoginR

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

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

-- instance YesodAuth App where
--    type AuthId App = UserId

    -- Where to send a user after successful login
--    loginDest _ = HomeR
    -- Where to send a user after logout
--    logoutDest _ = HomeR

--    getAuthId creds = runDB $ do
--        x <- getBy $ UniqueUser $ credsIdent creds
--        case x of
--            Just (Entity uid _) -> return $ Just uid
--            Nothing -> do
--                fmap Just $ insert User
--                    { userIdent = credsIdent creds
--                    , userPassword = Nothing
--                    }

    -- You can add other plugins like BrowserID, email or OAuth here
--    authPlugins _ = [authBrowserId def]

--    authHttpManager = httpManager

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
