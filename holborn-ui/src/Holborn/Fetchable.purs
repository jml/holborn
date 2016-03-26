module Holborn.Fetchable where

import Prelude
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax as AJ
import Web.Cookies as C
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, class DecodeJson)
import Data.Either (Either(..))
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

-- | Route changes generally imply the need to fetch fresh data from
-- the server (e.g. account data) but we don't know what routes and
-- nested state we have in advance (they can be nested). So they can
-- implement how to fetch themselves via this class.
--
-- We can check state before making a decision.
-- TODO fetching can always fail so the return value should be an Either
-- so that downstream can deal with the error.
class Fetchable action state where
  fetch :: forall eff. action -> state -> Aff (ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) state


type Fetch eff state = Aff (ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) state


decodeResponse :: forall eff result. (DecodeJson result) => AJ.AffjaxResponse Json -> Aff eff result
decodeResponse r = case
  decodeJson r.response of
    Left err -> unsafeThrow err
    Right x -> pure x
