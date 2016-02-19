module Holborn.Routing where

import Prelude

import Data.Functor ((<$))
import Control.Apply ((<*))
import Control.Alt ((<|>))
import Routing.Match (Match)
import Routing.Match.Class (lit)
import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Network.HTTP.Affjax as AJ
import Data.List (List)
import Holborn.ManualEncoding.Keys (Key)
import Data.Argonaut.Decode (decodeJson)


data RootRoutes =
    EmptyRoute
  | KeySettings
  | KeySettingsOK (List Key)
  | Route404
  | ErrorRoute


rootRoutes :: Match RootRoutes
rootRoutes =
  KeySettings <$ lit "settings" <* lit "keys"
  <|> pure Route404


fetchData :: forall eff. RootRoutes -> Aff (ajax :: AJ.AJAX | eff) RootRoutes
fetchData KeySettings = do
  r <- AJ.get "http://127.0.0.1:8002/v1/users/tom/keys"
  return $ case decodeJson r.response of
    Left err -> ErrorRoute
    Right keys -> KeySettingsOK keys

-- General case: Return route unmodified
fetchData x = return x
