module Holborn.Repo.Config
       ( Config(..)
       , buildRepoPath
       , warpSettings
       ) where

import BasicPrelude
import Data.Text (unpack)
import Network.Wai.Handler.Warp (Port, Settings, setPort, defaultSettings, setBeforeMainLoop)
import qualified Holborn.Logging as Log
import Holborn.JSON.RepoMeta (RepoId)
import Web.HttpApiData (toUrlPiece)


data Config = Config
    { repoRoot :: String
      -- ^ path to a set of bare repositories with the subdirectory
      -- shape ./org/repo
    , port :: Port
      -- ^ port to listen on for web API requests
    , rawPort :: Port
      -- ^ port to listen on for raw Git requests
    }

-- | Build a full repository path
-- TODO - find a better way to represent repos and conversions from /
-- to file system paths.
buildRepoPath :: Config -> RepoId -> String
buildRepoPath config repoId =
    concat [ repoRoot config
           , "/"
           , unpack (toUrlPiece repoId)
           ]


-- XXX: Duplicated & modified from holborn-syntax Main
-- | Generate warp settings from config
--
-- Serve from a port and print out where we're serving from.
warpSettings :: Config -> Settings
warpSettings config =
  setBeforeMainLoop printPort (setPort port' defaultSettings)
  where
    printPort = Log.info $ "holborn-repo running at http://localhost:" ++ show port' ++ "/"
    port' = port config
