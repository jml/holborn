{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Holborn.Repo
  ( RepoAPI
  , repoAPI
  , repoServer
  ) where

import Servant ((:>), (:<|>)(..), Capture, Proxy(..), Server)

import Holborn.JSON.RepoMeta (RepoId)
import Holborn.Repo.Browse (BrowseAPI, codeBrowser)
import Holborn.Repo.Config (Config(..))
import Holborn.Repo.GitLayer (makeRepository)
import Holborn.Repo.HttpProtocol
  ( DiskLocation(..),
    GitProtocolAPI
  , gitProtocolAPI
  , diskLocationToPath
  )


-- | The git pull & push repository API.
--
-- Repositories have a repoId, and each repository has an API for browsing and
-- one for the Git HTTP protocol.
type RepoAPI =
    "v1"
    :> "repos"
    :> Capture "repoId" RepoId
    :> (BrowseAPI :<|> GitProtocolAPI)

repoAPI :: Proxy RepoAPI
repoAPI = Proxy

repoServer :: Config -> Server RepoAPI
repoServer Config{repoRoot} repoId =
    codeBrowser repo :<|> gitProtocolAPI diskLocation
    where
      diskLocation = DiskLocation repoRoot repoId
      repo = makeRepository repoId (diskLocationToPath diskLocation)
