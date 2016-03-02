module Components.Router where

import Prelude
import Thermite as T
import React.DOM as R
import React.DOM.Props as RP
import React as React
import Network.HTTP.Affjax as AJ
import Control.Monad.Eff.Exception as E
import Control.Monad.Eff.Console (CONSOLE())

import Holborn.SettingsRoute as SettingsRoute
import Holborn.Signin as Signin


import Holborn.Routing (RootRoutes(..), rootRoutes)
import Web.Cookies as C
import Data.Maybe (Maybe(..))
import Routing (matches)
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Network.HTTP.Affjax (AJAX)
import Data.Lens(PrismP, prism, over)
import Data.Foldable (fold)
import Data.Either (Either(..))
import Holborn.Fetchable (fetch)


type State = { currentRoute :: RootRoutes }


data Action =
  UpdateRoute RootRoutes
  | SigninAction Signin.Action
  | SettingsAction SettingsRoute.Action


initialState :: State
initialState = { currentRoute: EmptyRoute}


_SigninState :: PrismP State Signin.State
_SigninState = prism (\s -> initialState { currentRoute = SigninRoute s }) \state ->
  case state.currentRoute of
    SigninRoute s -> Right s
    _ -> Left state

_SigninAction :: PrismP Action Signin.Action
_SigninAction = prism SigninAction \action ->
  case action of
    SigninAction x -> Right x
    _ -> Left action

_SettingsState :: PrismP State SettingsRoute.State
_SettingsState = prism (\s -> initialState { currentRoute = Settings s s.route}  ) \state ->
  case state.currentRoute of
    Settings s route -> Right (s { route = route })
    _ -> Left state

_SettingsAction :: PrismP Action SettingsRoute.Action
_SettingsAction = prism SettingsAction \action ->
  case action of
    SettingsAction x -> Right x
    _ -> Left action


spec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) State props Action
spec = container $ handleActions $ fold
       [ T.split _SigninState (T.match _SigninAction Signin.spec)
       , T.split _SettingsState (T.match _SettingsAction  SettingsRoute.spec)
       ]
  where
    container = over T._render \render d p s c ->
      [ R.div [RP.className "container-fluid"] (render d p s c) ]

    handleActions = over T._performAction \nestedPerformAction a p s k -> do
      nestedPerformAction a p s k
      handleAction a p s k

    -- TODO error handling when fetch fails
    handleAction action@(UpdateRoute r) p s k = do
      runAff (\err -> k id) (\result -> k \s -> s { currentRoute = result }) (fetch r)
    handleAction _ _ _ _ = pure unit


-- The following is a hack to listen on route changes for the "root"
-- component that controls everything else. `dispatch` can be
-- extracted from the spec but takes a `this` pointer which is only
-- valid once we mounted a component.
componentDidMount :: forall props eff. (React.ReactThis props State -> Action -> T.EventHandler)
                     -> React.ComponentDidMount props State (console :: CONSOLE, ajax :: AJAX, cookie :: C.COOKIE | eff)
componentDidMount dispatch this = do
    matches rootRoutes callback
  where
    callback :: forall eff2 refs. Maybe RootRoutes -> RootRoutes
                -> Eff ( props :: React.ReactProps
                       , state :: React.ReactState React.ReadWrite
                       , refs :: React.ReactRefs refs
                       , ajax :: AJAX
                       , cookie :: C.COOKIE | eff2) Unit
    callback _ rt = do
      maybeToken <- C.getCookie "auth-token"
      case maybeToken of
        -- force sign-in
        Nothing -> dispatch this (UpdateRoute (SigninRoute Signin.initialState))
        Just _ -> dispatch this (UpdateRoute rt)


component :: forall props. React.ReactClass props
component =
  let rspec = T.createReactSpec spec initialState
  in React.createClass
     ((_.spec rspec) { componentDidMount = (componentDidMount (_.dispatcher rspec)) })
