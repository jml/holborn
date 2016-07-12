module Holborn.Proxy.Config
  ( loadConfig
  , Config(..)
  , oauth2FromConfig
  , ServiceBaseUrl
  ) where

import BasicPrelude

import Options.Applicative
  ( ParserInfo
  , auto
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , progDesc
  , str
  , value
  , ReadM
  , eitherReader
  )
import qualified Network.Wai.Handler.Warp as Warp
import Network.OAuth.OAuth2 (OAuth2(..))


bs :: ReadM ByteString
bs = eitherReader parseUrl
  where
    parseUrl :: String -> Either String ByteString
    parseUrl s = pure (encodeUtf8 (fromString s))



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
loadConfig = execParser options

options :: ParserInfo Config
options =
  info (helper <*> parser) description
  where
    parser =
      Config
      <$> option auto
          ( long "port"
            <> metavar "PORT"
            <> help "http port to listen on"
            <> value 8080 )
      <*> option auto
          ( long "ssl-port"
            <> metavar "SSL_PORT"
            <> help "SSL Port to listen on"
            <> value 443 )
      <*> option str
          ( long "ssl-full-chain"
            <> metavar "HOLBORN_SSL_FULL_CHAIN"
            <> help "Absolute path to fullchain.pem (from acme)" )
      <*> option str
          ( long "ssl-key"
            <> metavar "HOLBORN_SSL_KEY"
            <> help "Absolute path to key.pem  (from acme)" )
      <*> option bs
          ( long "public-host"
            <> metavar "HOLBORN_PUBLIC_HOST"
            <> help "Public base url including https://"
            <> value "https://127.0.0.1:8443" )
      <*> option bs
          ( long "upstream-host"
            <> metavar "HOLBORN_UPSTREAM_HOST"
            <> help "Where to proxy to (including port)"
            <> value "127.0.0.1:8002" )
      <*> option auto
          ( long "upstream-port"
            <> metavar "HOLBORN_UPSTREAM_PORT"
            <> help "upstream port"
            <> value 8002 )
      <*> option bs
          ( long "dex-host"
            <> metavar "HOLBORN_DEX_HOST"
            <> help "Where dex lives (including port)"
            <> value "norf.co:5556" )
      <*> option bs
          ( long "oauth-client-id"
            <> metavar "HOLBORN_OAUTH_CLIENT_ID"
            <> help "OAuth2 client id. Dex prints this when registering a client." )
      <*> option bs
          ( long "oauth-client-secret"
            <> metavar "HOLBORN_OAUTH_CLIENT_SECRET"
            <> help "OAuth2 client secret. Dex prints this when registering a client." )

    description = concat
      [ fullDesc
      , progDesc "http/https terminator that also redirects to https and handles ACME requests."
      , header "holborn-proxy - our all-in-one http terminator"
      ]



oauth2FromConfig :: Config -> OAuth2
oauth2FromConfig Config{..} =
    OAuth2 { oauthClientId = configOauthClientId
           , oauthClientSecret = configOauthClientSecret
           , oauthCallback = Just (configPublicHost <> "/oauth2/callback")
           , oauthOAuthorizeEndpoint = configDexHost <> "/auth"
           , oauthAccessTokenEndpoint = configDexHost <> "/token"
           }
