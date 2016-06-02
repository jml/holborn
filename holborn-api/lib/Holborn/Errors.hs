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

import Data.Aeson (object)

import Holborn.API.Internal (HTTPCode, JSONCodeableError(..), jsonErrorHandler)


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


