module Components.Router where

import Prelude
import qualified Thermite as T
import qualified React.DOM as R
import qualified React as R
import qualified React.DOM.Props as RP
import qualified Thermite.Aff as TA
import qualified Network.HTTP.Affjax as AJ
import Control.Monad.Aff
import qualified Control.Monad.Eff.Exception as E
import Network.HTTP.StatusCode (StatusCode(..))
import Control.Monad.Eff.Console (CONSOLE())


import Data.Foreign (Foreign())
import Holborn.Routing (RootRoutes(..), rootRoutes)
import qualified Web.Cookies as C
import Data.Maybe
import Routing (matches)


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
    pickRoute KeySettings = [ R.text "KeySettings..." ]

    performAction action@(UpdateRoute r) props state k = k $ state { currentRoute = r }


-- The following is a hack to listen on route changes for the "root"
-- component that controls everything else. `dispatch` can be
-- extracted from the spec but takes a `this` pointer which is only
-- valid once we mounted a component.
componentDidMount :: forall props state eff. (React.ReactThis props state -> Action -> T.EventHandler) -> R.ComponentDidMount props state (console :: CONSOLE | eff)
componentDidMount dispatch this = do
    matches rootRoutes callback
  where
    callback :: Maybe RootRoutes -> RootRoutes -> T.EventHandler
    callback _ rt = do
      dispatch this (UpdateRoute rt)


component =
  -- Demo for how to hook into life cycle.
  let rspec = T.createReactSpec spec initialState
  in R.createClass ((_.spec rspec) { componentDidMount = (componentDidMount (_.dispatcher rspec))})
