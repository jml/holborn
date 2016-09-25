{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | Internal logic for how API requests work.
module Holborn.API.Internal
  ( APIHandler
  , runAPIHandler
  -- | Manipulate the database
  , IntegrityError(..)
  , query
  , queryWith
  , execute
  , executeWith
  , sql
  -- | Call backends
  , rjsonGet'
  -- | Repository server access
  , RepoAccess(..)
  , pickRepoServer
  , repoApiUrl
  , routeRepoRequest
  -- | Logging
  , corruptDatabase
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
import Data.Aeson (FromJSON, ToJSON, Value, eitherDecode', encode, object, (.=))
import qualified Database.PostgreSQL.Simple as PostgreSQL
import Database.PostgreSQL.Simple.Internal (RowParser)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)
import Network.HTTP.Client
  ( Manager
  , defaultManagerSettings
  , httpLbs
  , newManager
  , parseUrlThrow
  , requestHeaders
  , responseBody
  )
import Network.HTTP.Types.Header (hAccept)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (ServantErr(..), (:~>)(Nat), toUrlPiece)

import Holborn.API.Config (Config(..))
import Holborn.Logging (debugWithCallStack)
import Holborn.JSON.RepoMeta (OwnerName, RepoId, RepoName)
import Holborn.JSON.SSHRepoCommunication
  ( RepoCall(..)
  , GitCommand
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
    | NoUserAccount
      -- ^ Registered on dex but on the DB yet
  deriving (Show)


instance (JSONCodeableError a) => JSONCodeableError (APIError a) where
    toJSON MissingAuthToken = (401, object [])
    toJSON InvalidAuthToken = (401, object [])
    toJSON NoUserAccount = (418, object [])
    toJSON InsufficientPermissions = (403, object [])
    toJSON (SubAPIError x) = toJSON x
    toJSON (UnexpectedException e) = (500, object [ "message" .= (show e :: Text)
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
throwAPIError = APIHandler . lift . throwError


-- | The internal "configuration" of the application. Collects things used
-- across all handlers.
--
-- This is an implementation detail of 'APIHandler'. To create a
-- configuration, use 'Holborn.API.Config.Config'.
data AppConf = AppConf
  { conn :: PostgreSQL.Connection
  , httpManager :: Manager
  , repoHostname :: Text
    -- ^ Hostname for the repo server.
  , repoPort :: Warp.Port
    -- ^ Port for the repo server.
  , rawRepoHostname :: Text
    -- ^ Hostname for the raw repo server.
  , rawRepoPort :: Warp.Port
    -- ^ Port for the raw repo server.
  }

-- | Turn the pure configuration into something usable by the app.
loadAppConf :: Config -> IO AppConf
loadAppConf Config{..} =
  AppConf
    <$> PostgreSQL.connect (dbConnection)
    <*> newManager defaultManagerSettings
    <*> pure configRepoHostname
    <*> pure configRepoPort
    <*> pure configRepoHostname
    <*> pure configRawRepoPort


-- | Basic type for all of the handlers of Holborn.API.
newtype APIHandler err a =
  APIHandler { unwrapAPIHandler :: ReaderT AppConf (ExceptT (APIError err) IO) a }
  deriving (Functor, Applicative, Monad)

instance MonadFail (APIHandler err) where
  fail = throwAPIError . BrokenCode

-- | Run an API handler action without doing the error translation
runAPIHandler :: Config -> APIHandler err a -> ExceptT (APIError err) IO a
runAPIHandler config handler = do
  appConf <- liftIO $ loadAppConf config
  runReaderT (unwrapAPIHandler handler) appConf

-- | Turn a Holborn.API-specific handler into a generic Servant handler.
--
-- Use with 'enter'.
toServantHandler :: JSONCodeableError err => Config -> APIHandler err :~> ExceptT ServantErr IO
toServantHandler config = Nat toServantHandler'
  where
    toServantHandler' handler = bimapExceptT toServantErr id (runAPIHandler config handler)

-- | If something throws with 'throwM', we'll convert that to an unexpected exception.
instance MonadThrow (APIHandler err) where
  throwM e = throwAPIError (UnexpectedException (toException e))

-- | Load the application configuration
getConfig :: APIHandler err AppConf
getConfig = APIHandler ask


-- | Data integrity errors from Postgresql.
data IntegrityError = DuplicateValue | NotNullViolation deriving (Eq, Show)

-- | Query the database
query :: (PostgreSQL.ToRow values, PostgreSQL.FromRow row) => PostgreSQL.Query -> values -> APIHandler err [row]
query sqlQuery values = do
  AppConf{conn} <- getConfig
  APIHandler $ liftIO $ PostgreSQL.query conn sqlQuery values

-- | Query the database with a custom parser
queryWith :: (PostgreSQL.ToRow values) => RowParser row -> PostgreSQL.Query -> values -> APIHandler err [row]
queryWith parser sqlQuery values = do
  AppConf{conn} <- getConfig
  APIHandler $ liftIO $ PostgreSQL.queryWith parser conn sqlQuery values

-- | Perform an operation with a custom parser.
--
-- Data integrity errors are returned on the Left. Other errors raised as
-- exceptions.
executeWith :: (PostgreSQL.ToRow values) => RowParser row -> PostgreSQL.Query -> values -> APIHandler err (Either IntegrityError [row])
executeWith parser sqlQuery values = do
  AppConf{conn} <- getConfig
  APIHandler $ liftIO $ do
    catch (Right <$> PostgreSQL.queryWith parser conn sqlQuery values) $ \e ->
      case PostgreSQL.sqlState e of
        "23505" -> pure $ Left DuplicateValue
        "23502" -> pure $ Left NotNullViolation
        _ -> throwM e

-- | Execute an operation on the database, returning the number of rows affected.
execute :: PostgreSQL.ToRow values => PostgreSQL.Query -> values -> APIHandler err Int64
execute sqlQuery values = do
  AppConf{conn} <- getConfig
  APIHandler $ liftIO $ PostgreSQL.execute conn sqlQuery values


-- | Call a backend, asking for a rendered JSON response.
--
-- Will eagerly load the entire response into memory and convert all of the
-- JSON values to Haskell values.
rjsonGet' :: (FromJSON a) => Text -> APIHandler err (Either String a)
rjsonGet' endpoint = do
    AppConf{httpManager} <- getConfig
    r <- parseUrlThrow (textToString endpoint)
    let rJson = r { requestHeaders = [(hAccept, "application/r-json")] }
    APIHandler $ liftIO (httpLbs rJson httpManager) >>= \response -> return (eitherDecode' (responseBody response))


-- | Routing to a git repository.
data RepoAccess = AccessGranted Hostname Port RepoCall deriving (Show, Generic)

instance FromJSON RepoAccess
instance ToJSON RepoAccess

type Hostname = Text
type Port = Int

-- | Pick a repository server for storing a new repository
pickRepoServer :: APIHandler err Text
-- TODO: multi-repo-server: Getting repo server from config (which assumes
-- only one), should instead actually pick from a set of repo servers.
pickRepoServer = do
  AppConf{repoHostname, repoPort} <- getConfig
  pure $ repoHostname <> ":" <> fromShow repoPort

getRepoId :: OwnerName -> RepoName -> APIHandler err (Maybe RepoId)
getRepoId owner repo = do
  rows <- query [sql|
    select core_orgrepo.id from core_orgrepo, core_org
    where core_org.id = core_orgrepo.org_id and core_org.name = ? and core_orgrepo.name = ?
    UNION
    select core_userrepo.id from core_userrepo, auth_user
    where auth_user.id = core_userrepo.user_id and auth_user.username = ? and core_userrepo.name = ?
    |] (owner, repo, owner, repo)
  case rows of
    [[repoId :: RepoId]] -> pure (Just repoId)
    [] -> pure Nothing
    _ -> corruptDatabase $ show (length rows) <> " repositories found for " <> show owner <> "/" <> show repo

-- | Get the URL for the REST endpoint that serves the 'owner/repo'
-- repository. i.e. a URL for a holborn-repo server.
repoApiUrl :: OwnerName -> RepoName -> APIHandler err (Maybe Text)
repoApiUrl owner repo = do
  -- TODO: Because this is implemented only in terms of other things in
  -- Internal, it should probably be in a separate module, as it represents a
  -- higher layer. See
  -- https://bitbucket.org/holbornlondon/holborn/pull-requests/66/wip-notes-on-extracting-repo-layer/diff
  -- for discussion of this.
  repoId <- getRepoId owner repo
  case repoId of
    Nothing -> pure Nothing
    Just repoId' -> Just <$> repoUrlForId repoId'

repoUrlForId :: RepoId -> APIHandler err Text
repoUrlForId repoId = do
  -- TODO: multi-repo-server: Currently our config hardcodes that we have a
  -- single repo server. In future, we would look up the host details from the
  -- database too.
  AppConf{repoHostname, repoPort} <- getConfig
  pure $ "http://" <> repoHostname <> ":" <> fromShow repoPort <> "/v1/repos/" <> toUrlPiece repoId


-- | Given an SSH command line, return enough data to route the git traffic to
-- the correct repo on the correct repo server.
routeRepoRequest :: GitCommand -> OwnerName -> RepoName -> APIHandler err (Maybe RepoAccess)
routeRepoRequest command owner repo = do
  repoId <- getRepoId owner repo
  -- TODO: multi-repo-server: Currently, we hardcode a single repo server in
  -- the config. Should instead get the details from the database here.
  AppConf{rawRepoHostname, rawRepoPort} <- getConfig
  pure $ AccessGranted rawRepoHostname rawRepoPort . WritableRepoCall command <$> repoId


-- | Log something for debugging
logDebug :: (Show s, Stack.HasCallStack) => s -> APIHandler err ()
logDebug thing =
    APIHandler $ debugWithCallStack thing Stack.callStack


-- | We found something in the database that should never have been there in
-- the first place.
corruptDatabase :: Text -> a
corruptDatabase message = terror $ "ERROR: Corrupt database: " <> message
