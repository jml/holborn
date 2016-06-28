{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | Internal logic for how API requests work.
module Holborn.API.Internal
  ( APIHandler
  , runAPIHandler
  -- | Manipulate the database
  , query
  , execute
  -- | Call backends
  , jsonGet'
  -- | Repository server access
  , RepoAccess(..)
  , pickRepoServer
  , repoApiUrl
  , routeRepoRequest
  -- | Logging
  , logDebug
  -- | Integrate with Servant
  , toServantHandler
  -- | Error handling
  , JSONCodeableError(..)
  , throwHandlerError
  -- | Used in support code. Handlers should use throwHandlerError instead.
  , APIError(..)
  , throwAPIError
  ) where

import HolbornPrelude
import Control.Error (bimapExceptT)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (FromJSON, Value, eitherDecode', encode, object, (.=))
import qualified Database.PostgreSQL.Simple as PostgreSQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Network.HTTP.Client (parseUrl, httpLbs, requestHeaders, responseBody)
import Network.HTTP.Types.Header (hAccept)
import Servant (ServantErr(..), (:~>)(Nat), toUrlPiece)

import Holborn.API.Config (AppConf(..))
import Holborn.Logging (debugWithCallStack)
import Holborn.JSON.RepoMeta (OwnerName, RepoId, RepoName)
import Holborn.JSON.SSHRepoCommunication
  ( RepoCall(..)
  , SSHCommandLine(SSHCommandLine)
  )

import qualified GHC.Stack as Stack

type HttpCode = Int


-- | Servant doesn't do JSON errors out of the box, so we provide a type class
-- for converting errors to JSON.
class JSONCodeableError a where

    -- | Convert an error into an HTTP response code and a JSON value.
    toJSON :: a -> (HttpCode, Value)


-- | Convert a JSONCodeableError into a ServantErr
toServantErr :: JSONCodeableError err => err -> ServantErr
toServantErr err =
  ServantErr
  { errHTTPCode = code
  , errReasonPhrase = "error"
  , errHeaders = []
  , errBody = encode json
  }
  where
    (code, json) = toJSON err


-- | The full set of errors that can be returned by Holborn API.
--
-- Errors that can be raised in helper methods should be added directly to
-- this as extra constructors (see the auth-related constructors for
-- examples).
--
-- Errors specific to a particular handler can be raised using SubAPIError.
data APIError a =
      SubAPIError a
      -- ^ An error specific to a handler.
    | InvalidAuthToken
      -- ^ Received an invalid authentication token.
    | InsufficientPermissions
      -- ^ Do not have sufficient permission to perform the requested action.
    | MissingAuthToken
      -- ^ No auth token found.
    | UnexpectedException SomeException
      -- ^ Some code threw an exception that we couldn't understand
    | BrokenCode String
      -- ^ We called 'fail' when we should not have
  deriving (Show)


instance (JSONCodeableError a) => JSONCodeableError (APIError a) where
    toJSON MissingAuthToken = (401, object [])
    toJSON InvalidAuthToken = (401, object [])
    toJSON InsufficientPermissions = (403, object [])
    toJSON (SubAPIError x) = toJSON x
    toJSON (UnexpectedException e) = (500, object [ "message" .= show e
                                                  , "type" .= ("UncaughtException" :: Text)
                                                  ])
    toJSON (BrokenCode msg) = (500, object [ "message" .= msg
                                           , "type" .= ("BrokenCode" :: String)
                                           ])


-- | Indicate that a handler has failed for a reason specific to that handler.
throwHandlerError :: err -> APIHandler err a
throwHandlerError = throwAPIError . SubAPIError


-- | Raise a general API error.
throwAPIError :: APIError err -> APIHandler err a
throwAPIError = APIHandler . lift . throwE


-- | Basic type for all of the handlers of Holborn.API.
newtype APIHandler err a =
  APIHandler { unwrapAPIHandler :: ReaderT AppConf (ExceptT (APIError err) IO) a }
  deriving (Functor, Applicative, Monad)

instance MonadFail (APIHandler err) where
  fail = throwAPIError . BrokenCode

-- | Run an API handler action without doing the error translation
runAPIHandler :: AppConf -> APIHandler err a -> ExceptT (APIError err) IO a
runAPIHandler appConf handler = runReaderT (unwrapAPIHandler handler) appConf

-- | Turn a Holborn.API-specific handler into a generic Servant handler.
--
-- Use with 'enter'.
toServantHandler :: JSONCodeableError err => AppConf -> APIHandler err :~> ExceptT ServantErr IO
toServantHandler appConf = Nat (toServantHandler' appConf)

toServantHandler' :: JSONCodeableError err => AppConf -> APIHandler err a -> ExceptT ServantErr IO a
toServantHandler' appConf handler = bimapExceptT toServantErr id (runAPIHandler appConf handler)

-- | If something throws with 'throwM', we'll convert that to an unexpected exception.
instance MonadThrow (APIHandler err) where
  throwM e = throwAPIError (UnexpectedException (toException e))

-- | Load the application configuration
getConfig :: APIHandler err AppConf
getConfig = APIHandler ask


-- | Query the database
query :: (PostgreSQL.ToRow values, PostgreSQL.FromRow row) => PostgreSQL.Query -> values -> APIHandler err [row]
query sqlQuery values = do
  AppConf{conn} <- getConfig
  APIHandler $ liftIO $ PostgreSQL.query conn sqlQuery values

-- | Execute an operation on the database, returning the number of rows affected.
execute :: PostgreSQL.ToRow values => PostgreSQL.Query -> values -> APIHandler err Int64
execute sqlQuery values = do
  AppConf{conn} <- getConfig
  APIHandler $ liftIO $ PostgreSQL.execute conn sqlQuery values


-- | Call a backend, asking for a JSON response.
--
-- Will eagerly load the entire response into memory and convert all of the
-- JSON values to Haskell values.
jsonGet' :: (FromJSON a) => Text -> APIHandler err (Either String a)
jsonGet' endpoint = do
    AppConf{httpManager} <- getConfig
    r <- parseUrl (textToString endpoint)
    let rJson = r { requestHeaders = [(hAccept, "application/json")] }
    APIHandler $ liftIO (httpLbs rJson httpManager) >>= \response -> return (eitherDecode' (responseBody response))


-- | Routing to a git repository.
data RepoAccess = AccessGranted Hostname Port RepoCall deriving (Show)

type Hostname = Text
type Port = Int

-- | Pick a repository server for storing a new repository
pickRepoServer :: APIHandler err Text
-- TODO: multi-repo-server: Getting repo server from config (which assumes
-- only one), should instead actually pick from a set of repo servers.
pickRepoServer = do
  AppConf{repoHostname, repoPort} <- getConfig
  pure $ repoHostname <> ":" <> fromShow repoPort

-- | Get the URL for the REST endpoint that serves the 'owner/repo'
-- repository. i.e. a URL for a holborn-repo server.
repoApiUrl :: OwnerName -> RepoName -> APIHandler err Text
repoApiUrl owner repo = do
  -- TODO: Because this is implemented only in terms of other things in
  -- Internal, it should probably be in a separate module, as it represents a
  -- higher layer. See
  -- https://bitbucket.org/holbornlondon/holborn/pull-requests/66/wip-notes-on-extracting-repo-layer/diff
  -- for discussion of this.
  [(_ :: String, repoId :: RepoId)] <- query [sql|
    select "org_repo".id from "org_repo", "org"
    where "org".id = "org_repo".org_id and "org".orgname = ? and "org_repo".name = ?
    UNION
    select "user_repo".id from "user_repo", "user"
    where "user".id = "user_repo".user_id and "user".username = ? and "user_repo".name = ?
    |] (owner, repo, owner, repo)

  -- TODO: multi-repo-server: Currently our config hardcodes that we have a
  -- single repo server. In future, we would look up the host details from the
  -- database too.
  AppConf{repoHostname, repoPort} <- getConfig
  pure $ "http://" <> repoHostname <> ":" <> fromShow repoPort <> "/" <> toUrlPiece repoId

-- | Given an SSH command line, return enough data to route the git traffic to
-- the correct repo on the correct repo server.
routeRepoRequest :: SSHCommandLine -> APIHandler err RepoAccess
routeRepoRequest (SSHCommandLine command owner repo) = do
  -- TODO - the following is just a placeholder query so we can get
  -- a repoId. It works but needs error handling (return e.g. 404
  -- when repo wasn't found).
  [(_ :: String, repoId :: RepoId)] <- query [sql|
    select 'org', id from "org" where orgname = ? and name = ?
    UNION
    select 'user',  id from "user" where username = ? and name = ?
    |] (owner, repo, owner, repo)
  -- TODO: multi-repo-server: Currently, we hardcode a single repo server in
  -- the config. Should instead get the details from the database here.
  AppConf{rawRepoHostname, rawRepoPort} <- getConfig
  pure $ AccessGranted rawRepoHostname rawRepoPort (WritableRepoCall command repoId)


-- | Log something for debugging
logDebug :: (Show s, Stack.HasCallStack) => s -> APIHandler err ()
logDebug thing =
    APIHandler $ debugWithCallStack thing Stack.callStack
