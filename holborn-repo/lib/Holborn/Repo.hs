{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Holborn.Repo
  ( API
  , server
  ) where

import Servant ((:>), (:<|>)(..), Capture, Server)

import Holborn.JSON.RepoMeta (RepoId)
import qualified Holborn.Repo.Browse as Browse
import qualified Holborn.Repo.SearchAPI as Search
import Holborn.Repo.Config (Config)
import Holborn.Repo.Filesystem (diskLocationToPath, getLocation)
import Holborn.Repo.GitLayer (makeRepository)
import qualified Holborn.Repo.HttpProtocol as GitProtocol


-- | The git pull & push repository API.
--
-- Repositories have a repoId, and each repository has an API for browsing and
-- one for the Git HTTP protocol.
type SubAPIs =
  "browse" :> Browse.API
  :<|> "search" :> Search.API
  :<|> GitProtocol.API


type API =
    "v1"
    :> "repos"
    :> Capture "repoId" RepoId
    :> SubAPIs

server :: Config -> Server API
server config repoId =
    Browse.server repo :<|> Search.server repo :<|> GitProtocol.server diskLocation
    where
      diskLocation = getLocation config repoId
      repo = makeRepository repoId (diskLocationToPath diskLocation)
