{-# LANGUAGE TypeOperators      #-}

-- | Internal logic for how API requests work.
module Holborn.API.Internal
  ( HTTPCode
  , JSONCodeableError(..)
  , jsonErrorHandler
  ) where

import BasicPrelude
import Control.Error (bimapExceptT)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (Value, encode)
import Servant (ServantErr(..), (:~>)(Nat))


type HTTPCode = Int


class JSONCodeableError a where
    toJSON :: a -> (HTTPCode, Value)


jsonErrorHandler :: JSONCodeableError err => ExceptT err IO :~> ExceptT ServantErr IO
jsonErrorHandler = Nat (bimapExceptT handleError id)
  where
    handleError err = let (code, json) = toJSON err in ServantErr
      { errHTTPCode = code
      , errReasonPhrase = "error"
      , errHeaders = []
      , errBody = encode json
      }
