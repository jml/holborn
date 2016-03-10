module Holborn.Browse where

import Prelude
import Thermite as T
import Control.Monad.Eff.Exception as E
import Control.Apply ((*>))
import Control.Alt ((<|>))
import Network.HTTP.Affjax as AJ
import Routing.Match (Match)
import Routing.Match.Class (lit, str, fail)
import Web.Cookies as C
import React.DOM as R
import React.DOM.Props as RP


data State = State { route :: BrowseRoutes }
startRoute s = State { route: s }

data Action = NOP


data BrowseRoutes =
  Home String String


browseRoutes :: Match State
browseRoutes =
  map startRoute (Home <$> str <*> str)


spec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) State props Action
spec = T.simpleSpec T.defaultPerformAction render
  where
    render dispatch _ (State { route = Home org repo }) _ =
      [ R.h1 [] [R.text "browse"]
      , R.text $ org ++ repo
      ]
    render dispatch _ _ _ =
      [ R.text "browse"
      ]
