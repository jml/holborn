module Holborn.Routing where

import Prelude

import Data.Functor ((<$))
import Control.Apply ((<*))
import Control.Alt ((<|>))

import Routing
import Routing.Match
import Routing.Match.Class

data RootRoutes =
    EmptyRoute
  | KeySettings
  | Route404


rootRoutes :: Match RootRoutes
rootRoutes =
  KeySettings <$ lit "settings" <* lit "keys"
  <|> pure EmptyRoute
  <|> pure Route404
