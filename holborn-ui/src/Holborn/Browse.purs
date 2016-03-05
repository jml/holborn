module Holborn.Browse where

import Prelude
import Control.Apply ((*>))
import Control.Alt ((<|>))
import Routing.Match (Match)
import Routing.Match.Class (lit, str, fail)


data State = State { route :: BrowseRoutes }
startRoute s = State { route: s }

data Action = NOP


data BrowseRoutes =
  Home String String


browseRoutes :: Match State
browseRoutes =
  map startRoute (Home <$> str <*> str)
