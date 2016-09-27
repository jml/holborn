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

import Holborn.API.Config (Config)
import Holborn.API.Types (Username)
import Holborn.API.Internal
  ( APIHandler
  , JSONCodeableError(..)
  , toServantHandler
  , throwHandlerError
  , rjsonGet'
  , repoApiUrl
  , logDebug
  )
import Holborn.JSON.Browse (BrowseMetaResponse(..))
import Holborn.CommonTypes.Repo (OwnerName, RepoName)
import Holborn.ServantTypes (RenderedJson)

-- Following imports needed for RPC which we should do in a more
-- clever way (e.g. cereal library will be 100x faster)
import qualified Data.Time as Time

type API =
         Header "x-dex-name" Username
         :> Capture "owner" OwnerName
         :> Capture "repo" RepoName
         :> Get '[JSON] BrowseMetaResponse
    :<|> Header "x-dex-name" Username
         :> Capture "owner" OwnerName
         :> Capture "repo" RepoName
         :> "search"
         :> QueryParam "q" Text
         :> Get '[JSON, RenderedJson] Value
    :<|> Header "x-dex-name" Username
         :> Capture "owner" OwnerName
         :> Capture "repo" RepoName
         :> CaptureAll "pathspec" Text
         :> Get '[JSON, RenderedJson] Value

data BrowseError = NotFound | InvalidSearchQuery

instance JSONCodeableError BrowseError where
    toJSON NotFound = (404, object ["message" .= ("could not find object" :: Text)])
    toJSON InvalidSearchQuery = (400, object ["message" .= ("Invalid search query" :: Text)])


server :: Config -> Server API
server conf =
  enter (toServantHandler conf) $
  browse
  :<|> search
  :<|> treeCommitBlob



browse :: Maybe Username -> OwnerName -> RepoName -> APIHandler BrowseError BrowseMetaResponse
browse _maybeUsername owner repo = do
    repoUrl <- maybe (throwHandlerError NotFound) pure =<< repoApiUrl owner repo
    r <- rjsonGet' (repoUrl <> "/browse")
    repoMeta <- case r of
        Right x -> pure x
        Left err -> do
            logDebug ("Error when decoding JSON from repo backend at" :: String, repoUrl, err)
            throwHandlerError NotFound
    -- TODO: FAKE: Fake description in repository metadata
    return BrowseMetaResponse
      { repo_meta = repoMeta
      , description = "fake description"
      , created_at = Time.UTCTime (Time.ModifiedJulianDay 2000) (Time.secondsToDiffTime 10)
      }


-- Search is passed straight through if it meets the authentication
-- requirements.
search :: Maybe Username -> OwnerName -> RepoName -> Maybe Text -> APIHandler BrowseError Value
search _maybeUsername owner repo query  = do
  repoUrl <- maybe (throwHandlerError NotFound) pure =<< repoApiUrl owner repo
  q <- maybe (throwHandlerError InvalidSearchQuery) pure query

  -- TODO this should probably be an RPC request.
  let repoUrlBrokenAndHardcoded = repoUrl <> "/search?q=" <> q
  r <- rjsonGet' repoUrlBrokenAndHardcoded
  r' <- case r of
    Right x -> pure x
    Left err -> do
      logDebug ("Error when decoding JSON from repo backend at" :: String, repoUrlBrokenAndHardcoded, err)
      throwHandlerError NotFound
  return r'


-- Tree, commit & blob are passed straight through if they meet the
-- authentication requirements.
treeCommitBlob :: Maybe Username -> OwnerName -> RepoName -> [Text] -> APIHandler BrowseError Value
treeCommitBlob _maybeUsername owner repo pathspec = do
    repoUrl <- maybe (throwHandlerError NotFound) pure =<< repoApiUrl owner repo
    -- TODO: constructing this URL manually is still not great. A
    -- secondary concern is that we're decoding, then re-encoding JSON
    -- here. Might be easier to pipe through backend responses unmodified.
    let repoUrlBrokenAndHardcoded = repoUrl <> "/browse/" <> intercalate "/" pathspec
    r <- rjsonGet' repoUrlBrokenAndHardcoded
    r' <- case r of
        Right x -> pure x
        Left err -> do
            logDebug ("Error when decoding JSON from repo backend at" :: String, repoUrlBrokenAndHardcoded, err)
            throwHandlerError NotFound
    return r'
