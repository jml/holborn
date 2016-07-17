-- | Generic oauth2-validating proxy. This code replaces nginx and
-- oauth2 proxy a the same time.
--
-- TODO
-- * limit request sizes
-- * test with websockets (I *think* connection upgrades work with wpsUpgradeToRaw)
-- * timeouts
-- * logging
-- * exposing stats
-- * make dex store username on top of email (https://github.com/coreos/dex/issues/113)
--   alternative: make people choose username lazily after signup
--
-- If the user has authenticated with dex we'll store some credentials
-- and forward the following headers (values after : are examples).
--
--   X-Real-IP: 127.0.0.1
--   x-holborn-name:
--   x-holborn-email: t4@x.com
--   x-holborn-email-verified: True
--   x-forwarded-for: 127.0.0.1:45690
--
-- Additional user info goes into specific apps (e.g. admin app could
-- just hard-code list of admins, holborn-api has a parallel user
-- table that stores whether the user has been banned, etc).

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import BasicPrelude

import Crypto.JOSE (decodeCompact)
import Crypto.JWT (JWT(..), ClaimsSet(_unregisteredClaims))
import Data.Aeson (fromJSON, Result(..))
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Client (newManager, Manager, defaultManagerSettings)
import Network.HTTP.ReverseProxy (defaultOnExc, waiProxyTo, WaiProxyResponse(..), ProxyDest(..))
import Network.HTTP.Types (status302)
import Network.OAuth.OAuth2 (authorizationUrl, OAuth2(..), appendQueryParam, AccessToken(idToken), fetchAccessToken)
import Network.Wai (Request, requestHeaders, responseLBS, remoteHost, Application)
import Network.Wai.Handler.Warp (run, setPort, defaultSettings)
import Web.Cookie (parseCookies, renderSetCookie, def, setCookieName, setCookieValue, setCookiePath, setCookieSecure)
import Servant (serve, (:<|>)(..), (:>), Raw, Server, QueryParam, Header, Get, NoContent(..), JSON)
import Servant.Server (ServantErr(..), err302, err401)
import Data.Proxy (Proxy(..))
import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Data.HashMap.Strict as HashMap
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Servant.Utils.StaticFiles (serveDirectory)
import Control.Concurrent (forkIO, threadDelay)

import Holborn.Proxy.Config (loadConfig, Config(..), oauth2FromConfig, ServiceBaseUrl)
import Holborn.Proxy.AuthJar (AuthJar(..), UserCookie, TrustedCreds(..), newMemoryJar)


type ProxyAPI =
    "oauth2" :> "callback" :> QueryParam "code" Text :> Get '[JSON] NoContent
    :<|> Header "Cookie" Text :> Raw


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


redirectAndAcmeServer :: ServiceBaseUrl -> Server RedirectAndAcmeAPI
redirectAndAcmeServer baseUrl =
    serveDirectory "/var/www/challenges"
    :<|> redirectToHTTPS
    where
      -- TODO append path from request (e.g. http://bla.co/path -> https://bla.co/path)
      redirectToHTTPS _ respond = respond (responseLBS status302 [("location", baseUrl)] BSL.empty)


-- | Redirect to dex (or whatever we hav configured)
redirectToToAuth :: OAuth2 -> Application
redirectToToAuth oauth2Conf _ respond =
    let redirectUrl = authorizationUrl oauth2Conf `appendQueryParam` [("scope", "openid profile email")]
    in respond (responseLBS status302 [("location", redirectUrl)] BSL.empty)


-- | Set the cookie that will identify the user on subsequent
-- requests. Note that it's not HTTPOnly because we want to use it
-- from JS as well.
redirectSetAuthCookie :: Config -> UserCookie -> ExceptT ServantErr IO NoContent
redirectSetAuthCookie Config{configPublicHost} userCookie =
    -- TODO redirectUrl should be a type etc.
    let redirectUrl = configPublicHost
        authCookie = def { setCookieName = "auth_cookie", setCookieValue = userCookie, setCookiePath = Just "/", setCookieSecure = False }
        setCookie = (BSL.toStrict . toLazyByteString . renderSetCookie) authCookie
    in throwE (err302 { errHeaders = [("Set-Cookie", setCookie), ("location", redirectUrl)] }) >> pure NoContent


-- | Tell the user what went wrong. At some point we probably want to
-- send a generic 401 and log in parallel for debugging.
authProblem :: Text -> ExceptT ServantErr IO NoContent
authProblem msg =
    throwE (err401 { errBody = BSL.fromStrict (encodeUtf8 msg)}) >> pure NoContent


handleOauth2Callback :: (AuthJar jar) => Config -> Manager -> jar -> Maybe Text -> ExceptT ServantErr IO NoContent
handleOauth2Callback config@Config{..} manager jar maybeCode = do
    -- Classic Control.Monad.when doesn't work because it's () instead of NoContent
    _ <- if (maybeCode == Nothing) then authProblem "need code" else pure NoContent
    let Just code = maybeCode
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

  where
      -- TODO: unpackClaims is nested too deeply but checked by
      -- compiler and no IO so I assume it works..
      unpackClaims :: AccessToken -> Either String TrustedCreds
      unpackClaims accessToken =
        case idToken accessToken of
          Just id_ ->
              -- PUPPY I don't *think* we need to verify the JWT
              -- access token because it's been given to us by
              -- dex. Might be wrong though. If we do need to check we
              -- need to send a verify request as well and use the
              -- keys from the response to verify the token.
              case decodeCompact (BSL.fromStrict id_) of
                Right jwt ->
                  case emailEtcClaims (jwtClaimsSet (jwt :: JWT)) of
                    Nothing -> Left (textToString ("missing claims. Token: " <> (show jwt)))
                    Just claims ->
                      case claims of
                        Success creds -> Right creds
                        Error err -> Left err
                _ -> Left "Could not decode claims"
          Nothing -> Left "missing id_token"

      emailEtcClaims claimsSet = do
          let c = _unregisteredClaims claimsSet
          email <- fmap fromJSON (HashMap.lookup "email" c)
          email_verified <- (fmap fromJSON (HashMap.lookup "email_verified" c)) <|> Just (Success False)
          name <- fmap fromJSON (HashMap.lookup "name" c)
          pure $ TrustedCreds <$> email <*> email_verified <*> name


handleProxying :: (AuthJar jar) => Config -> Manager -> jar -> Maybe Text -> Application
handleProxying config@Config{..} manager jar cookie = waiProxyTo doProxy defaultOnExc manager
  where
    doProxy :: Request -> IO WaiProxyResponse
    doProxy request = do
      case cookie >>= \cookie' -> lookup "auth_cookie" (parseCookies (encodeUtf8 cookie')) of
        Nothing -> pure (WPRApplication (redirectToToAuth (oauth2FromConfig config)))
        Just authCookie -> do
            c <- get jar authCookie
            case c of
              Just userCookie -> pure (setCredHeaders request userCookie)
              Nothing -> pure (WPRApplication (redirectToToAuth (oauth2FromConfig config)))

    setCredHeaders :: Request -> TrustedCreds -> WaiProxyResponse
    setCredHeaders request' TrustedCreds{..} =
        let headers = requestHeaders request'
            maybeHeader hdr = (,) hdr <$> lookup hdr headers
            forwardedHeaders =
              [ ("x-holborn-name", encodeUtf8 _name)
              , ("x-holborn-email", encodeUtf8 _email)
              , ("x-holborn-email-verified", encodeUtf8  (show _emailVerified))
              , ("x-forwarded-for", encodeUtf8  (show (remoteHost request'))) -- TODO maybe needs better encoding
              ] ++ (catMaybes
                     [ maybeHeader "Accept"
                     , maybeHeader "Accept-Encoding"
                     , maybeHeader "Content-Length"
                     , maybeHeader "Content-Type"
                     , maybeHeader "If-Modified-Since"
                     , maybeHeader "If-None-Match"
                     , maybeHeader "User-Agent"
                     ])
        in WPRModifiedRequest (request' {requestHeaders = forwardedHeaders}) (ProxyDest configUpstreamHost configUpstreamPort)


app :: (AuthJar jar) => Config -> Manager -> jar -> Application
app config manager jar = serve proxyApi (proxyServer config manager jar)


main :: IO ()
main = do
    config@Config{..} <- loadConfig
    -- TOOD we might want an on-disk jar to survive restarts. Not sure
    -- what the security trade-offs are here.
    jar <- newMemoryJar
    manager <- newManager defaultManagerSettings

    void $ forkIO $ run 8080 (serve redirectAndAcmeAPI (redirectAndAcmeServer configPublicHost))

    -- TODO 14th: run tls port, install this thing on norf.co
    let settings = setPort configSslPort defaultSettings
    forever $ do
        runTLS (tlsSettings configSslFullChain configSslKey) settings (app config manager jar) `catch` degradedModeMessage
        threadDelay (1000 * 5000)
  where
    degradedModeMessage (err :: IOException) = do
        putStrLn "Error when running TLS server:"
        print (show err)
        putStrLn "Running in degraded mode."
