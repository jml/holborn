module Components.Router where

import Prelude
import Thermite as T
import React.DOM as R
import React as React
import Network.HTTP.Affjax as AJ
import Control.Monad.Eff.Exception as E
import Control.Monad.Eff.Console (CONSOLE())

import Holborn.KeySettings as KeySettings

import Holborn.Routing (RootRoutes(..), rootRoutes, fetchData)
import Web.Cookies as C
import Data.Maybe (Maybe)
import Routing (matches)
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Network.HTTP.Affjax (AJAX)


import Debug.Trace (traceAnyM)

type State =
  { currentRoute :: RootRoutes
  }


data Action =
  UpdateRoute RootRoutes

initialState :: State
initialState =
  { currentRoute: EmptyRoute
  }


spec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) State props Action
spec = T.simpleSpec performAction render
  where
    render :: T.Render State props Action
    render dispatch _ s _ = pickRoute s.currentRoute

    pickRoute EmptyRoute = [ R.text "loading..." ]
    pickRoute Route404 = [ R.text "404 not found" ]
    pickRoute (KeySettingsOK keys) = [ KeySettings.component {keys: keys} ]
    pickRoute ErrorRoute = [ R.text "error" ]
    pickRoute _ = [ R.text "404 not found" ]

    performAction action@(UpdateRoute r) props state k = k $ state { currentRoute = r }


-- The following is a hack to listen on route changes for the "root"
-- component that controls everything else. `dispatch` can be
-- extracted from the spec but takes a `this` pointer which is only
-- valid once we mounted a component.
componentDidMount :: forall props state eff. (React.ReactThis props state -> Action -> T.EventHandler)
                     -> React.ComponentDidMount props state (console :: CONSOLE, ajax :: AJAX | eff)
componentDidMount dispatch this = do
    matches rootRoutes callback
  where
    callback :: forall eff refs. Maybe RootRoutes -> RootRoutes
                -> Eff (props :: React.ReactProps, state :: React.ReactState React.ReadWrite, refs :: React.ReactRefs refs, ajax :: AJAX | eff) Unit
    callback _ rt = do

      -- Fetch route async or sync
      runAff
        (\err -> traceAnyM err >>= \_ -> dispatch this (UpdateRoute ErrorRoute))
        (dispatch this <<< UpdateRoute)
        (fetchData rt)

      -- dispatch this (UpdateRoute rt)


component :: forall props. React.ReactClass props
component =
  -- Demo for how to hook into life cycle.
  let rspec = T.createReactSpec spec initialState
  in React.createClass ((_.spec rspec) { componentDidMount = (componentDidMount (_.dispatcher rspec))})
