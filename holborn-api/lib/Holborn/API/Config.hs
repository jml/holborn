-- | Configuration for the API server.

module Holborn.API.Config ( Config(..) ) where

import HolbornPrelude

import Database.PostgreSQL.Simple (ConnectInfo)
import qualified Network.Wai.Handler.Warp as Warp


-- | "Pure" configuration that can be loaded from the environment, a config
-- file, etc.
data Config = Config { port :: Warp.Port
                     , dbConnection :: ConnectInfo
                     , configRepoHostname :: Text
                     , configRepoPort :: Warp.Port
                     , configRawRepoPort :: Warp.Port
                     } deriving Show
