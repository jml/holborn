module Holborn.Routing where

import Prelude

import Data.Functor ((<$))
import Control.Apply ((<*))
import Control.Alt ((<|>))

import Routing.Match (Match)
import Routing.Match.Class (lit)

import Data.Foreign.Class (readJSON, IsForeign, readProp)
import Network.HTTP.Affjax as AJ
import Control.Monad.Aff (Aff)
import Data.Either (Either(..))

import Data.Foreign (Foreign)

data Key =
  Key { key :: String
      , title :: String
      }

-- TODO. We're decoding JSON manually, we want to move to
-- auto-generated decoders ASAP (probably with servant-foreign).
instance objectIsForeign :: IsForeign Key where
  read value = do
    key <- readProp "key" value
    title <- readProp "title" value
    return $ Key { key: key, title: title }


data RootRoutes =
    EmptyRoute
  | KeySettings
  | KeySettingsOK (Array Key)
  | Route404
  | ErrorRoute


rootRoutes :: Match RootRoutes
rootRoutes =
  KeySettings <$ lit "settings" <* lit "keys"
  <|> pure Route404


fetchData :: forall eff. RootRoutes -> Aff (ajax :: AJ.AJAX | eff) RootRoutes
fetchData KeySettings = do
  r <- AJ.get "http://127.0.0.1:8002/v1/users/tom/keys"
  return $ case readJSON r.response of
    Left err -> ErrorRoute
    Right keys -> KeySettingsOK keys

-- General case: Return route unmodified
fetchData x = return x
