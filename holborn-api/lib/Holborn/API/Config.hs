-- | Configuration for the API server.

module Holborn.API.Config ( Config(..) ) where

import HolbornPrelude
import GHC.Word (Word16)
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
                     , configRawRepoPort :: Warp.Port
                     } deriving Show
