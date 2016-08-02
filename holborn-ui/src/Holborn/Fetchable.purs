module Holborn.Fetchable where

import Prelude
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax as AJ
import Web.Cookies as C
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, class DecodeJson)
import Data.Either (Either(..))
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import DOM (DOM)

-- | Route changes generally imply the need to fetch fresh data from
-- the server (e.g. account data) but we don't know what routes and
-- nested state we have in advance (they can be nested). So they can
-- implement how to fetch themselves via this class.
--
-- We can check state before making a decision.
-- TODO fetching can always fail so the return value should be an Either
-- so that downstream can deal with the error.

type Fetch eff state = Aff (ajax :: AJ.AJAX, cookie :: C.COOKIE, dom :: DOM | eff) state

class Fetchable action state where
  fetch :: forall eff. action -> state -> Fetch eff state


-- | Decode a JSON response in the Aff monad which is also an instance
-- of (MonadError Error e). Use this whenever possible so we have a
-- unified framework for handling e.g. 403s.
--
-- It's a bit lame that the Aff error is `Error` which is basically
-- the same as a javascript exception so we're really writing
-- throw/catch code here.
decodeResponse :: forall eff result. (DecodeJson result) => AJ.AffjaxResponse Json -> Aff eff result
decodeResponse r = case
  decodeJson r.response of
    Left err -> unsafeThrow err
    Right x -> pure x
