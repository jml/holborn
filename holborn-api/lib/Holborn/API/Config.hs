-- | Configuration for the API server.

module Holborn.API.Config ( AppConf(..)
                          , Config(..)
                          , loadAppConf
                          ) where

import BasicPrelude
import Database.PostgreSQL.Simple (Connection, connect, ConnectInfo(..), defaultConnectInfo)
import GHC.Word (Word16)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import qualified Network.Wai.Handler.Warp as Warp


-- | "Pure" configuration that can be loaded from the environment, a config
-- file, etc.
data Config = Config { port :: Warp.Port
                     , pgDb :: String
                     , pgUser :: String
                     , pgPort :: Word16
                     , configBaseUrl :: Text
                     , configStaticBaseUrl :: Text
                     , configRepoHostname :: Text
                     , configRepoPort :: Warp.Port
                     , configRawRepoHostname :: Text
                     , configRawRepoPort :: Warp.Port
                     } deriving Show


-- | Configuration usable directly by application.
data AppConf = AppConf
  { conn :: Connection
  , jwtSecret :: Text
  , httpManager :: Manager
  , baseUrl :: Text -- e.g. https://holborn-example.com/
  , staticBaseUrl :: Text -- e.g. https://holborn-example.com/
  , repoHostname :: Text
    -- ^ Hostname for the repo server.
  , repoPort :: Warp.Port
    -- ^ Port for the repo server.
  , rawRepoHostname :: Text
    -- ^ Hostname for the raw repo server.
  , rawRepoPort :: Warp.Port
    -- ^ Port for the raw repo server.
  }


-- | Turn the pure configuration into something usable by the app.
loadAppConf :: Config -> IO AppConf
loadAppConf Config{..} =
  AppConf
    <$> connect (defaultConnectInfo  { connectDatabase = pgDb, connectUser = pgUser, connectPort = pgPort })
    <*> pure "test-secret-todo-read-from-env"
    <*> newManager defaultManagerSettings
    <*> pure configBaseUrl
    <*> pure configStaticBaseUrl
    <*> pure configRepoHostname
    <*> pure configRepoPort
    <*> pure configRawRepoHostname
    <*> pure configRawRepoPort
