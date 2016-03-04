{-# LANGUAGE TypeOperators      #-}
-- | Servant doesn't do JSON errors out of the box but the API returns
-- only JSON errors.
--
-- To avoid having to define a global data type with all possible
-- errors we define a GenericError which is parametrized over
-- SpecificError, and then have a type class that allows errors to
-- encode themselves as JSON.

module Holborn.Errors
       ( HTTPCode
       , JSONCodeableError(..)
       , APIError(..)
       , jsonErrorHandler
       ) where

import BasicPrelude
import Control.Error (bimapExceptT)
import Servant (enter, ServantErr(..), (:~>)(Nat))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson (Value(..), object, encode)
import Control.Monad.Trans.Either (EitherT(..))


type HTTPCode = Int
class JSONCodeableError a where
    toJSON :: a -> (HTTPCode, Value)


data APIError a =
      SubAPIError a
    | InvalidAuthToken
    | InsufficientPermissions
    | MissingAuthToken


instance (JSONCodeableError a) => JSONCodeableError (APIError a) where
    toJSON MissingAuthToken = (401, object [])
    toJSON InvalidAuthToken = (401, object [])
    toJSON InsufficientPermissions = (403, object [])
    toJSON (SubAPIError x) = toJSON x


jsonErrorHandler :: JSONCodeableError err => ExceptT err IO :~> EitherT ServantErr IO
jsonErrorHandler = Nat (exceptTToEitherT . bimapExceptT handleError id)
  where
    exceptTToEitherT = EitherT . runExceptT
    handleError err = let (code, json) = toJSON err in ServantErr
      { errHTTPCode = code
      , errReasonPhrase = "error"
      , errHeaders = []
      , errBody = encode json
      }
