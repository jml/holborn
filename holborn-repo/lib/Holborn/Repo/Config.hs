module Holborn.Repo.Config
       ( Config(..)
       , buildRepoPath
       ) where

import BasicPrelude
import Data.Text (unpack)

data Config = Config
    { repoRoot :: String
      -- ^ path to a set of bare repositories with the subdirectory
      -- shape ./org/repo
    }

-- | Build a full repository path
-- TODO - find a better way to represent repos and conversions from /
-- to file system paths.
buildRepoPath :: Config -> Text -> Text -> String
buildRepoPath (Config root) userOrOrg repo =
    concat [ root
           , "/"
           , unpack userOrOrg
           , "/"
           , unpack repo
           ]
