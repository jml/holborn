{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Holborn.Proxy.HttpTermination
  ( ProxyAPI
  , RedirectAndAcmeAPI
  , proxyApp
  , redirectApp
  ) where

import HolbornPrelude

import Control.Error (hoistMaybe)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Client (Manager)
import Network.HTTP.ReverseProxy (defaultOnExc, waiProxyTo, WaiProxyResponse(..), ProxyDest(..))
import Network.HTTP.Types (status302, status200)
import Network.OAuth.OAuth2 (authorizationUrl, OAuth2(..), appendQueryParam, fetchAccessToken)
import Network.Wai (Request, requestHeaders, responseLBS, remoteHost, Application, rawPathInfo, rawQueryString, responseFile)
import Web.Cookie (parseCookies, renderSetCookie, def, setCookieName, setCookieValue, setCookiePath, setCookieSecure)
import Servant (serve, (:<|>)(..), (:>), Raw, Server, QueryParam, Header, Get, NoContent(..), JSON)
import Servant.Server (ServantErr(..), err302, err401)
import Data.Proxy (Proxy(..))
import Control.Monad.Trans.Except (ExceptT, throwE)
import Servant.Utils.StaticFiles (serveDirectory)

import Holborn.Proxy.Config (Config(..), oauth2FromConfig, ServiceBaseUrl)
import Holborn.Proxy.AuthJar (AuthJar(..), UserCookie, TrustedCreds, unpackClaims, trustedCredsHeaders)


type ProxyAPI =
    "oauth2" :> "callback" :> QueryParam "code" Text :> Get '[JSON] NoContent
    :<|> "v1" :> Header "Cookie" Text :> Raw
    -- default handler serves /static content + index.html on any path
    :<|> "static" :> Raw
    :<|> Raw


type RedirectAndAcmeAPI =
    ".well-known" :> "acme-challenge" :> Raw
    :<|> Raw


proxyApi :: Proxy ProxyAPI
proxyApi = Proxy


redirectAndAcmeAPI :: Proxy RedirectAndAcmeAPI
redirectAndAcmeAPI = Proxy


proxyServer :: (AuthJar jar) => Config -> Manager -> jar -> Server ProxyAPI
proxyServer config manager jar =
  handleOauth2Callback config manager jar
  :<|> handleProxying config manager jar
  :<|> serveDirectory "/run/current-system/sw/ui/static"
  -- TODO: we need to set headers in such a way that index.html is always reloaded
  -- nginx uses sth like `expired no-cache no-store private auth;`
  :<|> \_ respond -> respond (responseFile status200 [] "/run/current-system/sw/ui/index.html" Nothing)


redirectAndAcmeServer :: ServiceBaseUrl -> Server RedirectAndAcmeAPI
redirectAndAcmeServer baseUrl =
    serveDirectory "/var/www/challenges"
    :<|> redirectToHTTPS
    where
      -- We're appending the full request path and query string on
      -- redirect for a nicer user experience.
      redirectToHTTPS request respond = respond (
          responseLBS status302 [("location", (encodeUtf8 (show baseUrl)) <> (rawPathInfo request) <> (rawQueryString request))] BSL.empty)


-- | Redirect to dex (or whatever oauth2 provider we have configured)
redirectToToAuth :: OAuth2 -> Application
redirectToToAuth oauth2Conf _ respond =
    let redirectUrl = authorizationUrl oauth2Conf `appendQueryParam` [("scope", "openid profile email")]
    in respond (responseLBS status302 [("location", redirectUrl)] BSL.empty)


-- | Set the cookie that will identify the user on subsequent
-- requests. Note that it's not HTTPOnly because we want to use it
-- from JS as well.
redirectSetAuthCookie :: Config -> UserCookie -> ExceptT ServantErr IO NoContent
redirectSetAuthCookie Config{configPublicHost} userCookie =
    let redirectUrl = encodeUtf8 (show configPublicHost)
        authCookie = def { setCookieName = "auth_cookie", setCookieValue = userCookie, setCookiePath = Just "/", setCookieSecure = False }
        setCookie = (BSL.toStrict . toLazyByteString . renderSetCookie) authCookie
    in throwE (err302 { errHeaders = [("Set-Cookie", setCookie), ("location", redirectUrl)] }) >> pure NoContent


-- | Tell the user what went wrong. At some point we probably want to
-- send a generic 401 and log in parallel for debugging.
authProblem :: Text -> ExceptT ServantErr IO NoContent
authProblem msg = throwE (err401 { errBody = BSL.fromStrict (encodeUtf8 msg)})


handleOauth2Callback :: (AuthJar jar) => Config -> Manager -> jar -> Maybe Text -> ExceptT ServantErr IO NoContent
handleOauth2Callback _ _ _ Nothing = authProblem "need code"
handleOauth2Callback config@Config{..} manager jar (Just code) = do
    eitherToken <- liftIO (fetchAccessToken manager (oauth2FromConfig config) (encodeUtf8 code))
    case eitherToken of
      Right token -> do
          let trustedCreds = unpackClaims token
          case trustedCreds of
            Right creds -> do
                userCookie <- liftIO (make jar)
                liftIO (set jar userCookie creds)
                redirectSetAuthCookie config userCookie
            Left err -> authProblem ("could not parse id_token: " <> (show err))
      Left err -> liftIO (print err) >> authProblem "token problem"


handleProxying :: (AuthJar jar) => Config -> Manager -> jar -> Maybe Text -> Application
handleProxying config@Config{..} manager jar cookie = waiProxyTo doProxy defaultOnExc manager
  where
    authRedirect = pure (WPRApplication (redirectToToAuth (oauth2FromConfig config)))
    doProxy :: Request -> IO WaiProxyResponse
    doProxy request = do
      userCookie <- getUserCookie
      case userCookie of
        Just userCookie' -> pure (setCredHeaders request userCookie')
        Nothing -> authRedirect

    getUserCookie :: IO (Maybe TrustedCreds)
    getUserCookie = runMaybeT $ do
      cookie' <- hoistMaybe cookie
      authCookie <- hoistMaybe $ lookup "auth_cookie" (parseCookies (encodeUtf8 cookie'))
      userCookie <- lift $ get jar authCookie
      hoistMaybe userCookie

    setCredHeaders :: Request -> TrustedCreds -> WaiProxyResponse
    setCredHeaders request' trustedCreds =
        let headers = requestHeaders request'
            forwardedHeaders  =
              (trustedCredsHeaders trustedCreds) ++ [
              ("x-forwarded-for", encodeUtf8  (show (remoteHost request'))) -- TODO maybe needs better encoding
              ] ++ (whitelistHeaders headers
              [ "Accept"
              , "Accept-Encoding"
              , "Content-Length"
              , "Content-Type"
              , "If-Modified-Since"
              , "If-None-Match"
              , "User-Agent"
              ] )
        in WPRModifiedRequest (request' {requestHeaders = forwardedHeaders}) (ProxyDest configUpstreamHost configUpstreamPort)

    whitelistHeaders headers whitelist = catMaybes (map maybeHeader whitelist)
      where
        maybeHeader hdr = (,) hdr <$> lookup hdr headers



proxyApp :: (AuthJar jar) => Config -> Manager -> jar -> Application
proxyApp config manager jar = serve proxyApi (proxyServer config manager jar)


redirectApp :: ServiceBaseUrl -> Application
redirectApp configPublicHost =
  serve redirectAndAcmeAPI (redirectAndAcmeServer configPublicHost)
