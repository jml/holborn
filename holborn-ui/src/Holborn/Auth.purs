module Holborn.Auth where

import Prelude

import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax (Affjax, URL, defaultRequest, affjax)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.Method (Method(POST))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Web.Cookies as C
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error)
import Network.HTTP.StatusCode (StatusCode(..))


authTokenMissing = error "auth-token cookie missing. This is unexpected because you should't see the signed in page without this cookie."

-- | Custom post that sends the correct auth header derived from the
-- COOKIE.
post :: forall e a b. (Requestable a, Respondable b) => URL -> a -> Affjax (cookie :: C.COOKIE | e)  b
post u c = do
  maybeToken <- liftEff $ C.getCookie "auth-token"
  t <- case maybeToken of
    Nothing -> throwError authTokenMissing
    Just x -> return x
  affjax $ defaultRequest
    { method = POST
    , url = u
    , content = Just c
    , headers = [RequestHeader "Authorization" t ]
    }

-- | Custom get that sends the correct auth header derived from the
-- COOKIE.
get :: forall e a. (Respondable a) => URL -> Affjax (cookie :: C.COOKIE | e) a
get u = do
  maybeToken <- liftEff $ C.getCookie "auth-token"
  t <- case maybeToken of
    Nothing -> throwError authTokenMissing
    Just x -> return x
  affjax $ defaultRequest
    { url = u
    , headers = [RequestHeader "Authorization" t]
    }
