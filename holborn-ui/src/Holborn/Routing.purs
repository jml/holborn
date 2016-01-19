module Holborn.Routing where

import Prelude

import Data.Functor ((<$))
import Control.Alt ((<|>))
import Routing
import Routing.Match
import Routing.Match.Class

data RootRoutes = RouteA String | RouteB

rootRoutes :: Match RootRoutes
rootRoutes = RouteA <$ lit "a" <*> str <|> RouteB <$ lit "b"
