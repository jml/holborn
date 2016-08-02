module Holborn.Auth where

import Prelude

import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax (Affjax, URL, defaultRequest, affjax)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax.Response (class Respondable)
import Data.HTTP.Method (Method(POST, DELETE))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error)
import Network.HTTP.StatusCode (StatusCode(..))
import Data.Either (Either(Left))



-- | Custom post that sends the correct auth header derived from the
-- COOKIE.
post :: forall e a b. (Requestable a, Respondable b) => URL -> a -> Affjax e b
post u c = do
  affjax $ defaultRequest
    { method = Left POST
    , url = u
    , content = Just c
    , headers = [RequestHeader "Accept" "application/json"]
    }

-- | Custom get that sends the correct auth header derived from the
-- COOKIE.
get :: forall e a. (Respondable a) => URL -> Affjax e a
get u = do
  affjax $ defaultRequest
    { url = u
    , headers = [RequestHeader "Accept" "application/json"]
    }

getRendered :: forall e a. (Respondable a) => URL -> Affjax e a
getRendered u = do
  affjax $ defaultRequest
    { url = u
    , headers = [RequestHeader "Accept" "application/r-json"]
    }

delete :: forall e b. (Respondable b) => URL -> Affjax e  b
delete u = do
  affjax $ defaultRequest
    { method = Left DELETE
    , url = u
    , headers = [RequestHeader "Accept" "application/json"]
    }
