{-# LANGUAGE TypeOperators      #-}

-- | Internal logic for how API requests work.
module Holborn.API.Internal
  ( APIHandler
  , getConfig
  -- | Manipulate the database
  , query
  , execute
  , JSONCodeableError(..)
  , toServantHandler
  , throwAPIError
  , throwHandlerError
  -- | Used in support code. Handlers should use the above instead.
  , APIError(..)
  ) where

import BasicPrelude
import Control.Error (bimapExceptT)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (Value, encode, object)
import qualified Database.PostgreSQL.Simple as PostgreSQL
import Servant (ServantErr(..), (:~>)(Nat))

import Holborn.API.Config (AppConf(..))


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

instance (JSONCodeableError a) => JSONCodeableError (APIError a) where
    toJSON MissingAuthToken = (401, object [])
    toJSON InvalidAuthToken = (401, object [])
    toJSON InsufficientPermissions = (403, object [])
    toJSON (SubAPIError x) = toJSON x


-- | Indicate that a handler has failed for a reason specific to that handler.
throwHandlerError :: (JSONCodeableError err) => err -> APIHandler err a
throwHandlerError = throwAPIError . SubAPIError


-- | Raise a general API error.
throwAPIError :: APIError err -> APIHandler err a
throwAPIError = APIHandler . lift . throwE


-- | Basic type for all of the handlers of Holborn.API.
--
-- TODO: This really shouldn't derive MonadIO, but rather instead we should
-- export functions that do whatever we need IO for.
newtype APIHandler err a =
  APIHandler { runAPIHandler :: ReaderT AppConf (ExceptT (APIError err) IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)


-- | Load the application configuration
getConfig :: APIHandler err AppConf
getConfig = APIHandler ask


-- | Query the database
query :: (PostgreSQL.ToRow values, PostgreSQL.FromRow row) => PostgreSQL.Query -> values -> APIHandler err [row]
query sql values = do
  AppConf{conn} <- getConfig
  APIHandler $ liftIO $ PostgreSQL.query conn sql values

-- | Execute an operation on the database, returning the number of rows affected.
execute :: PostgreSQL.ToRow values => PostgreSQL.Query -> values -> APIHandler err Int64
execute sql values = do
  AppConf{conn} <- getConfig
  APIHandler $ liftIO $ PostgreSQL.execute conn sql values


-- | Turn a Holborn.API-specific handler into a generic Servant handler.
--
-- Use with 'enter'.
toServantHandler :: JSONCodeableError err => AppConf -> APIHandler err :~> ExceptT ServantErr IO
toServantHandler appConf = Nat (toServantHandler' appConf)


toServantHandler' :: JSONCodeableError err => AppConf -> APIHandler err a -> ExceptT ServantErr IO a
toServantHandler' appConf handler = bimapExceptT toServantErr id (runReaderT (runAPIHandler handler) appConf)
