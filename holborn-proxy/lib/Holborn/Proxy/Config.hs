module Holborn.Proxy.Config
  ( Config(..)
  , oauth2FromConfig
  , ServiceBaseUrl
  ) where

import HolbornPrelude

import qualified Network.Wai.Handler.Warp as Warp
import Network.OAuth.OAuth2 (OAuth2(..))
import Network.URI (URI)


type ServiceBaseUrl = URI

data Config = Config { configPort :: Warp.Port
                     , configPublicHost :: URI
                     , configUpstreamHost :: ByteString
                     , configUpstreamPort :: Warp.Port
                     , configDexHost :: ByteString
                     , configOauthClientId :: ByteString
                     , configOauthClientSecret :: ByteString
                     } deriving Show


oauth2FromConfig :: Config -> OAuth2
oauth2FromConfig Config{..} =
    OAuth2 { oauthClientId = configOauthClientId
           , oauthClientSecret = configOauthClientSecret
           , oauthCallback = Just ((encodeUtf8 (show configPublicHost)) <> "/oauth2/callback")
           , oauthOAuthorizeEndpoint = configDexHost <> "/auth"
           , oauthAccessTokenEndpoint = configDexHost <> "/token"
           }
