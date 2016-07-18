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
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import HolbornPrelude
import Holborn.Proxy.Config (Config(..))
import Holborn.Proxy.HttpTermination (proxyApp, redirectApp)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Network.Wai.Middleware.RequestLogger as RL
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
import Holborn.Proxy.AuthJar (newMemoryJar)
import Control.Concurrent (forkIO, threadDelay)
import Network.Wai.Handler.Warp (run, setPort, defaultSettings)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.URI (URI, parseURI)


loadConfig :: IO Config
loadConfig = execParser options

bs :: ReadM ByteString
bs = eitherReader parseBS
  where
    parseBS :: String -> Either String ByteString
    parseBS s = pure (encodeUtf8 (fromString s))

uri :: ReadM URI
uri = eitherReader parseUrl
  where
    parseUrl :: String -> Either String URI
    parseUrl s = case parseURI s of
      Nothing -> Left ("Could not parse URL: " <> s)
      Just uri' -> pure uri'

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
      <*> option uri
          ( long "public-host"
            <> metavar "HOLBORN_PUBLIC_HOST"
            <> help "Public base url including https://"
            <> let Just defURI = parseURI "https://127.0.0.1:8443" in value defURI )
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


main :: IO ()
main = do
    config@Config{..} <- loadConfig
    -- TOOD we might want an on-disk jar to survive restarts. Not sure
    -- what the security trade-offs are here.
    jar <- newMemoryJar
    manager <- newManager defaultManagerSettings

    void $ forkIO $ run configPort (RL.logStdoutDev (redirectApp configPublicHost))

    let settings = setPort configSslPort defaultSettings
    forever $ do
        runTLS (tlsSettings configSslFullChain configSslKey) settings (RL.logStdoutDev (proxyApp config manager jar)) `catch` degradedModeMessage
        threadDelay (1000 * 5000) -- 5 seconds
  where
    degradedModeMessage (err :: IOException) = do
        printErr "Error when running TLS server:"
        printErr (show err)
        printErr "Running in degraded mode."
