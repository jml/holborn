{-# LANGUAGE TypeOperators      #-}

-- | Internal logic for how API requests work.
module Holborn.API.Internal
  ( HTTPCode
  , JSONCodeableError(..)
  , toServantHandler
  ) where

import BasicPrelude
import Control.Error (bimapExceptT)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (Value, encode)
import Servant (ServantErr(..), (:~>)(Nat))


type HTTPCode = Int


class JSONCodeableError a where
    toJSON :: a -> (HTTPCode, Value)


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


-- | Turn a Holborn.API-specific handler into a generic Servant handler.
--
-- Use with 'enter'.
toServantHandler :: JSONCodeableError err => ExceptT err IO :~> ExceptT ServantErr IO
toServantHandler = Nat (bimapExceptT toServantErr id)
