-- | Generic oauth2-validating proxy. This code replaces nginx and
-- oauth2 proxy a the same time.
--
-- TODO
-- * limit request sizes
-- * test with websockets (I *think* connection upgrades work with wpsUpgradeToRaw)
-- * timeouts
-- * logging
-- * exposing stats
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
module Main (main) where

import BasicPrelude

import Crypto.JOSE (decodeCompact)
import Crypto.JWT (JWT(..), ClaimsSet(_unregisteredClaims))
import Data.Aeson (fromJSON, Result(..))
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import Network.HTTP.Client (newManager, Manager, defaultManagerSettings)
import Network.HTTP.ReverseProxy (defaultOnExc, waiProxyTo, WaiProxyResponse(..), ProxyDest(..))
import Network.HTTP.Types (status302, status401)
import Network.OAuth.OAuth2 (authorizationUrl, OAuth2(..), appendQueryParam, AccessToken(idToken), fetchAccessToken)
import Network.Wai (Request, requestHeaders, responseLBS, remoteHost, Application, pathInfo, queryString)
import Network.Wai.Handler.Warp (run)
import Web.Cookie (parseCookies, renderSetCookie, def, setCookieName, setCookieValue, setCookiePath, setCookieSecure)
import Control.Concurrent.STM.TVar (newTVarIO, modifyTVar', readTVarIO, TVar)
import Control.Concurrent.STM (atomically)
import GHC.Generics (Generic)


-- | TrustedCreds are extracted from the id_token returend by
-- fetchAccessToken and are therefore turstworthy.
data TrustedCreds = TrustedCreds
  { _email :: Text
  , _emailVerified :: Bool
  , _name :: Text
  } deriving (Show, Generic)

instance Hashable TrustedCreds


type UserCookie = ByteString
type CookieMap = HashMap.HashMap UserCookie TrustedCreds

data MemoryJar = MemoryJar (TVar CookieMap)

newMemoryJar :: IO MemoryJar
newMemoryJar = MemoryJar <$> newTVarIO HashMap.empty


class AuthJar a where
    get :: a -> UserCookie -> IO (Maybe TrustedCreds)
    set :: a -> UserCookie -> TrustedCreds -> IO ()
    make :: a -> IO UserCookie


-- | An in-memory jar for testing
instance AuthJar MemoryJar where
    set (MemoryJar jar) key token = atomically (modifyTVar' jar (HashMap.insert key token))
    get (MemoryJar jar) key = readTVarIO jar >>= \m -> pure (HashMap.lookup key m)
    make _ = pure "test-cookie" -- TODO random bytes


-- | Secret fluff and oauth2 URLs.
dexKey :: OAuth2
dexKey = OAuth2
         { oauthClientId = "HWBN_c1uNIItwm-gamnkZEhYeKDE-jKNmE_vJvq--BU=@127.0.0.1"
         , oauthClientSecret = "M7KgulFwBODJa5XObMNUqygJtqDXmGjk3Tuwfrwsdge_ffNJA3wM9qSUC2hCW2vjXnhDbNz4Qpt4aUAKONG2W-aPwMoMdCly"
         , oauthCallback = Just "http://127.0.0.1:8002/oauth2/callback"
         , oauthOAuthorizeEndpoint = "http://52.50.74.68:5556/auth"
         , oauthAccessTokenEndpoint = "http://52.50.74.68:5556/token"
         }

-- | Redirect to dex
redirectoToAuth :: Application
redirectoToAuth _ respond =
    let redirectUrl = authorizationUrl dexKey `appendQueryParam` [("scope", "openid profile email")]
    in respond (responseLBS status302 [("location", redirectUrl)] BSL.empty)


-- | Set the cookie that will identify the user on subsequent
-- requests. Note that it's not HTTPonly because we want to use it
-- from JS as well.
redirectSetAuthCookie :: UserCookie -> Application
redirectSetAuthCookie userCookie _ respond =
    let redirectUrl = "http://127.0.0.1:8002/"
        authCookie = def { setCookieName = "auth_cookie", setCookieValue = userCookie, setCookiePath = Just "/", setCookieSecure = False }
        setCookie = (BSL.toStrict . toLazyByteString . renderSetCookie) authCookie
    in respond (responseLBS status302 [("Set-Cookie", setCookie), ("location", redirectUrl)] BSL.empty)


-- | Tell the user what went wrong. At some point we probably want to
-- send a generic 401 and log in parallel for debugging.
authProblem :: Text -> WaiProxyResponse
authProblem msg = WPRApplication app
  where
    app _ respond =
      respond (responseLBS status401 [] (BSL.fromStrict  (encodeUtf8 msg)))

proxy :: (AuthJar jar) => Manager -> jar -> Request -> IO WaiProxyResponse
proxy manager jar request = do
    let headers = requestHeaders request

    case pathInfo request of
      ["oauth2", "callback"] -> do
          case lookup "code" (queryString request) of
            Just (Just code) -> do
                eitherToken <- fetchAccessToken manager dexKey code
                case eitherToken of
                  Right token -> do
                      let trustedCreds = unpackClaims token
                      case trustedCreds of
                        Right creds -> do
                            userCookie <- make jar
                            set jar userCookie creds
                            pure (WPRApplication (redirectSetAuthCookie userCookie))
                        Left _ -> pure (authProblem "could not parse id_token")
                  Left err -> print err >> pure (authProblem "token problem")
            _ -> pure (authProblem "invalid oauth2 code")

      _ -> case lookup "Cookie" headers >>= \cookies -> lookup "auth_cookie" (parseCookies cookies) of
             Nothing -> pure (WPRApplication redirectoToAuth)
             Just authCookie -> do
                 c <- get jar authCookie
                 case c of
                   Just userCookie -> pure (setCredHeaders request userCookie)
                   Nothing -> pure (WPRApplication redirectoToAuth)
    where
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
          in WPRModifiedRequest (request' {requestHeaders = forwardedHeaders}) (ProxyDest "127.0.0.1" 8000)

      -- TODO disgusting code but checked by compiler and no IO so I
      -- assume it works.
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
                    Nothing -> Left "missing claims"
                    Just claims ->
                      case claims of
                        Success creds -> Right creds
                        Error err -> Left err
                _ -> Left "Could not decode claims"
          Nothing -> Left "missing id_token"

      emailEtcClaims claimsSet = do
          let c = _unregisteredClaims claimsSet
          email <- fmap fromJSON (HashMap.lookup "email" c)
          email_verified <- fmap fromJSON (HashMap.lookup "email_verified" c)
          name <- fmap fromJSON (HashMap.lookup "name" c)
          pure $ TrustedCreds <$> email <*>  email_verified <*> name


main :: IO ()
main = do
    jar <- newMemoryJar
    manager <- newManager defaultManagerSettings
    let app = waiProxyTo (proxy manager jar) defaultOnExc manager
    run 8002 app
