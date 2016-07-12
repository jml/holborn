module Holborn.Proxy.Config
  ( loadConfig
  , Config(..)
  , oauth2FromConfig
  , ServiceBaseUrl
  ) where

import BasicPrelude

import qualified Env
import qualified Network.Wai.Handler.Warp as Warp
import Network.OAuth.OAuth2 (OAuth2(..))


type ServiceBaseUrl = ByteString

data Config = Config { configPort :: Warp.Port
                     , configSslPort :: Warp.Port
                     , configSslFullChain :: FilePath
                     , configSslKey :: FilePath
                     , configPublicHost :: ServiceBaseUrl
                     , configUpstreamHost :: ByteString
                     , configUpstreamPort :: Warp.Port
                     , configDexHost :: ByteString
                     , configOauthClientId :: ByteString
                     , configOauthClientSecret :: ByteString
                     } deriving Show

loadConfig :: IO Config
loadConfig =
  Env.parse (Env.header "holborn-proxy") $
  Config
  <$> Env.var Env.auto
      "PORT" (Env.def 80 <> Env.help "Port to listen on")
  <*> Env.var Env.auto
      "SSL_PORT" (Env.def 443 <> Env.help "SSL Port to listen on")
  <*> Env.var Env.str
      "HOLBORN_SSL_FULL_CHAIN" (Env.def "" <> Env.help "Absolute path to fullchain.pem (from acme)")
  <*> Env.var Env.str
      "HOLBORN_SSL_KEY" (Env.def "" <> Env.help "Absolute path to key.pem  (from acme)")
  <*> Env.var Env.str
      "HOLBORN_PUBLIC_HOST" (Env.def "127.0.0.1:8000" <> Env.help "Public IP (and port) of proxy (needed in some redirects so needs to be correct)")
  <*> Env.var Env.str
      "HOLBORN_UPSTREAM_HOST" (Env.def "127.0.0.1:8002" <> Env.help "Where to proxy to")
  <*> Env.var Env.auto
      "HOLBORN_UPSTREAM_PORT" (Env.def 8002 <> Env.help "Upstream port")
  <*> Env.var Env.str
      "HOLBORN_DEX_HOST" (Env.def "127.0.0.1:8005" <> Env.help "Where dex lives")
  <*> Env.var Env.str
      "HOLBORN_OAUTH_CLIENT_ID" (Env.help "OAuth2 client id. Dex prints this when registering a client.")
  <*> Env.var Env.str
      "HOLBORN_OAUTH_CLIENT_SECRET" (Env.help "OAuth2 client secret. Dex prints this when registering a client.")


oauth2FromConfig :: Config -> OAuth2
oauth2FromConfig Config{..} =
    OAuth2 { oauthClientId = configOauthClientId
           , oauthClientSecret = configOauthClientSecret
           , oauthCallback = Just (configPublicHost <> "/oauth2/callback")
           , oauthOAuthorizeEndpoint = configDexHost <> "/auth"
           , oauthAccessTokenEndpoint = configDexHost <> "/token"
           }
