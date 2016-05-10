-- | Configuration for the API server.

module Holborn.API.Config ( Config(..)
                          , loadAppConf
                          ) where

import BasicPrelude
import Database.PostgreSQL.Simple (connect, ConnectInfo(..), defaultConnectInfo)
import GHC.Word (Word16)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Network.Wai.Handler.Warp as Warp

import Holborn.API.Types (AppConf(AppConf))


-- | "Pure" configuration that can be loaded from the environment, a config
-- file, etc.
data Config = Config { port :: Warp.Port
                     , pgDb :: String
                     , pgUser :: String
                     , pgPort :: Word16
                     , baseUrl :: String
                     , staticBaseUrl :: String
                     } deriving Show


-- | Turn the pure configuration into something usable by the app.
loadAppConf :: Config -> IO AppConf
loadAppConf Config{..} =
  AppConf
    <$> connect (defaultConnectInfo  { connectDatabase = pgDb, connectUser = pgUser, connectPort = pgPort })
    <*> pure "test-secret-todo-read-from-env"
    <*> newManager defaultManagerSettings
    <*> pure (fromString baseUrl)
    <*> pure (fromString staticBaseUrl)
