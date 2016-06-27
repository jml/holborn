module Holborn.Repo.Config
       ( Config(..)
       , warpSettings
       ) where

import HolbornPrelude
import Network.Wai.Handler.Warp (Port, Settings, setPort, defaultSettings, setBeforeMainLoop)
import qualified Holborn.Logging as Log


data Config = Config
    { repoRoot :: String
      -- ^ path to a set of bare repositories with the subdirectory
      -- shape ./<repoid>
    , port :: Port
      -- ^ port to listen on for web API requests
    , rawPort :: Port
      -- ^ port to listen on for raw Git requests
    }


-- | Generate warp settings from config
--
-- Serve from a port and print out where we're serving from.
warpSettings :: Config -> Settings
warpSettings config =
  setBeforeMainLoop printPort (setPort port' defaultSettings)
  where
    printPort = Log.info $ "holborn-repo running at http://localhost:" ++ show port' ++ "/"
    port' = port config
