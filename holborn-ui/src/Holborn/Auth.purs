-- | Custom network functions that send the correct headers (and other
-- modififications as necessary).
-- TODO rename to RPC.purs (auth is done by browser cookie now)
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
import Data.Either (Either(..))
import Control.Monad.Eff.Exception (Error)
import Network.HTTP.Affjax as AJ
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Decode (decodeJson, class DecodeJson)


post :: forall e a b. (Requestable a, Respondable b) => URL -> a -> Affjax e b
post u c = do
  affjax $ defaultRequest
    { method = Left POST
    , url = u
    , content = Just c
    , headers = [RequestHeader "Accept" "application/json"]
    }

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


-- | Easier to process error that summarises what the end-user cares
-- about (invalid input, other problem, success).
data HandledResult a b c = FormError b | OtherError c | OK a

handleResult :: forall a b. (DecodeJson a, DecodeJson b) => Either Error (AJ.AffjaxResponse Json) -> HandledResult a b String
handleResult r = case r of
  Right {status: StatusCode 201, headers, response } ->
    case decodeJson response of
      Left err -> OtherError (show err)
      Right x -> OK x
  Right {status: StatusCode 400, headers, response } ->
    case decodeJson response of
      Left err -> OtherError (show err)
      Right x -> FormError x
  Left err -> OtherError (show err)
  Right {status: status, headers, response } -> OtherError ("unexpected response: " <> show status)
  _ -> OtherError "totally unexpected thing happened"
