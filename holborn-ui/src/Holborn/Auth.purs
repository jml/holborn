module Holborn.Auth where

import Prelude

import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax (Affjax, URL, defaultRequest, affjax)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax.Response (class Respondable)
import Data.HTTP.Method (Method(POST, DELETE))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Web.Cookies as C
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error)
import Network.HTTP.StatusCode (StatusCode(..))
import Data.Either (Either(Left))


getAuthHeader :: forall e. Eff (cookie :: C.COOKIE | e) (Array RequestHeader)
getAuthHeader = do
  maybeToken <- C.getCookie "auth-token"
  pure $ case maybeToken of
        Nothing -> []
        Just x -> [RequestHeader "Authorization" (x :: String)]


-- | Custom post that sends the correct auth header derived from the
-- COOKIE.
post :: forall e a b. (Requestable a, Respondable b) => URL -> a -> Affjax (cookie :: C.COOKIE | e) b
post u c = do
  authHeader <- liftEff getAuthHeader
  affjax $ defaultRequest
    { method = Left POST
    , url = u
    , content = Just c
    , headers = authHeader <> [RequestHeader "Accept" "application/json"]
    }

-- | Custom get that sends the correct auth header derived from the
-- COOKIE.
get :: forall e a. (Respondable a) => URL -> Affjax (cookie :: C.COOKIE | e) a
get u = do
  authHeader <- liftEff getAuthHeader
  affjax $ defaultRequest
    { url = u
    , headers = authHeader <> [RequestHeader "Accept" "application/json"]
    }

getRendered :: forall e a. (Respondable a) => URL -> Affjax (cookie :: C.COOKIE | e) a
getRendered u = do
  authHeader <- liftEff getAuthHeader
  affjax $ defaultRequest
    { url = u
    , headers = authHeader <> [RequestHeader "Accept" "application/r-json"]
    }

delete :: forall e b. (Respondable b) => URL -> Affjax (cookie :: C.COOKIE | e)  b
delete u = do
  authHeader <- liftEff getAuthHeader
  affjax $ defaultRequest
    { method = Left DELETE
    , url = u
    , headers = authHeader <> [RequestHeader "Accept" "application/json"]
    }
