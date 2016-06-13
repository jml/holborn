{-# LANGUAGE TypeOperators      #-}

-- | Internal logic for how API requests work.
module Holborn.API.Internal
  ( APIHandler
  , runAPIHandler
  -- | Get the application configuration
  , getConfig
  -- | Manipulate the database
  , query
  , execute
  -- | Call backends
  , jsonGet'
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
import Network.HTTP.Client (parseUrl, httpLbs, requestHeaders, responseBody)
import Network.HTTP.Types.Header (hAccept)
import Servant (ServantErr(..), (:~>)(Nat))

import Holborn.API.Config (AppConf(..))
import Holborn.Logging (debugWithCallStack)

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
query sql values = do
  AppConf{conn} <- getConfig
  APIHandler $ liftIO $ PostgreSQL.query conn sql values

-- | Execute an operation on the database, returning the number of rows affected.
execute :: PostgreSQL.ToRow values => PostgreSQL.Query -> values -> APIHandler err Int64
execute sql values = do
  AppConf{conn} <- getConfig
  APIHandler $ liftIO $ PostgreSQL.execute conn sql values


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


-- | Log something for debugging
logDebug :: (Show s, Stack.HasCallStack) => s -> APIHandler err ()
logDebug thing =
    APIHandler $ debugWithCallStack thing Stack.callStack
