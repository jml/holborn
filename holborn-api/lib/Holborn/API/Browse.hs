-- | Browse is responsible for routing traffic to the holborn-repo
-- server that's responsible for a given repository.
--
-- This is a prime candidate for moving routing decisions closer to
-- the edge but that's a premature optimization for now.
--
-- Test e.g.:
--   curl 127.0.0.1:8002/v1/browse -H "content-type: application/json" -d'{"repo": "jml/holborn", "path": ""}'
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE QuasiQuotes #-}

module Holborn.API.Browse
       ( API
       , server
       ) where

import HolbornPrelude

import Data.Aeson (object, (.=), Value)
import Servant

import Database.PostgreSQL.Simple.SqlQQ (sql)
import Holborn.API.Config (AppConf(..))
import Holborn.API.Types (Username)
import Holborn.API.Internal (APIHandler, JSONCodeableError(..), getConfig, toServantHandler, throwHandlerError, jsonGet', query, logDebug)
import Holborn.JSON.Browse (BrowseMetaResponse(..))
import Holborn.JSON.RepoMeta (RepoId, RepoMeta(..))


-- Following imports needed for RPC which we should do in a more
-- clever way (e.g. cereal library will be 100x faster)
import qualified Data.Time as Time

type Owner = Text
type Repo = Text

-- TODO: we need the CaptureAll combinator to capture paths of objects.
-- https://github.com/haskell-servant/servant/pull/519
-- Until we have that browsing will be brokwn
type API =
         Header "GAP-Auth" Username
         :> Capture "owner" Owner
         :> Capture "repo" Repo
         :> Get '[JSON] BrowseMetaResponse
    :<|> Header "GAP-Auth" Username
         :> Capture "owner" Owner
         :> Capture "repo" Repo
         :> "git" :> "trees" :> "master"
         :> Get '[JSON] Value
    :<|> Header "GAP-Auth" Username
         :> Capture "owner" Owner
         :> Capture "repo" Repo
         :> "git" :> "blobs"
         :> Get '[JSON] Value
    :<|> Header "GAP-Auth" Username
         :> Capture "owner" Owner
         :> Capture "repo" Repo
         :> "git" :> "commits"
         :> Get '[JSON] Value

data BrowseError = NotFound

instance JSONCodeableError BrowseError where
    toJSON NotFound = (404, object ["message" .= ("could not find object" :: Text)])


server :: AppConf -> Server API
server conf =
  enter (toServantHandler conf) (browse
  :<|> treeCommitBlob
  :<|> treeCommitBlob
  :<|> treeCommitBlob)


browse :: Maybe Username -> Owner -> Repo -> APIHandler BrowseError BrowseMetaResponse
browse _maybeUsername owner repo = do
    AppConf{repoHostname, repoPort} <- getConfig
    [(_ :: String, repoId :: RepoId)] <- query [sql|
               select 'org', "org_repo".id from "org_repo", "org"
                      where "org".id = "org_repo".org_id and "org".orgname = ? and "org_repo".name = ?
               UNION
               select 'user',  "user_repo".id from "user_repo", "user"
                      where "user".id = "user_repo".user_id and "user".username = ? and "user_repo".name = ?
               |] (owner, repo, owner, repo)

    let repoUrl = "http://" <> repoHostname <> ":" <> fromShow repoPort <> "/v1/repos/" <> toUrlPiece repoId
    r <- jsonGet' repoUrl
    repoMeta <- case r of
        Right x -> pure x
        Left err -> do
            logDebug ("Error when decoding JSON from repo backend at" :: String, repoUrl, err)
            throwHandlerError NotFound
    return BrowseMetaResponse
      { _BrowseMetaResponse_repo_meta = repoMeta { _RepoMeta_owner = owner }
      , _BrowseMetaResponse_description = "fake description"
      , _BrowseMetaResponse_created_at = Time.LocalTime (Time.ModifiedJulianDay 2000) (Time.TimeOfDay 1 1 1)
      }

-- Tree, commit & blob are passed straight through if they meet the
-- authentication requirements.
treeCommitBlob :: Maybe Username -> Owner -> Repo -> APIHandler BrowseError Value
treeCommitBlob _maybeUsername owner repo = do
    -- TODO read repoHostname from DB (we already have a column)
    AppConf{repoHostname, repoPort} <- getConfig
    [(_ :: String, repoId :: RepoId)] <- query [sql|
               select 'org', "org_repo".id from "org_repo", "org"
                      where "org".id = "org_repo".org_id and "org".orgname = ? and "org_repo".name = ?
               UNION
               select 'user',  "user_repo".id from "user_repo", "user"
                      where "user".id = "user_repo".user_id and "user".username = ? and "user_repo".name = ?
               |] (owner, repo, owner, repo)

    -- TODO wait for CaptureAll combinator so we can construct the correct path here
    let repoUrlBrokenAndHardcoded = "http://" <> repoHostname <> ":" <> fromShow repoPort <> "/v1/repos/" <> toUrlPiece repoId <> "/git/trees/master"
    r <- jsonGet' repoUrlBrokenAndHardcoded
    r' <- case r of
        Right x -> pure x
        Left err -> do
            logDebug ("Error when decoding JSON from repo backend at" :: String, repoUrlBrokenAndHardcoded, err)
            throwHandlerError NotFound
    return r'
