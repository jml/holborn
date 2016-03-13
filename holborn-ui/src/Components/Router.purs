module Components.Router where

import Prelude
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception as E
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor (($>))
import Data.Lens(PrismP, prism, over, lens, LensP, view, set)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax as AJ
import React as React
import React.DOM as R
import React.DOM.Props as RP
import Routing (matches)
import Routing.Match (Match)
import Routing.Match.Class (lit)
import Thermite as T
import Web.Cookies as C

import Holborn.Browse as Browse
import Holborn.Fetchable (class Fetchable, fetch)
import Holborn.SettingsRoute as SettingsRoute
import Holborn.Signin as Signin
import Debug.Trace

data State = RouterState { currentRoute :: RootRoutes, username :: String }


data RootRoutes =
    EmptyRoute
  | Route404
  | SigninRoute Signin.State
  | Settings SettingsRoute.State -- TODO fix inconsitent naming
  | BrowseRoute Browse.State

-- TODO tom: Routes should really be "invertible" so I can create a
-- KeySettings route string from the value.
rootRoutes :: Match RootRoutes
rootRoutes =
  lit "settings" *> (map Settings SettingsRoute.settingsRoutes)
  <|> (map BrowseRoute Browse.browseRoutes)
  <|> pure Route404


data Action =
  UpdateRoute RootRoutes
  | SigninAction Signin.Action
  | SettingsAction SettingsRoute.Action
  | BrowseAction Browse.Action


-- TODO: As jml observed fetching (and our entire app) is essentially
-- a state monad but it's a bit unclear how to fit that observation
-- into real code.
instance fetchRootRoutes :: Fetchable RootRoutes State where
  fetch route state@(RouterState { username = "anonymous" }) = do
    traceAnyM "not logged in do a pretend login as alice"
    -- if the user fetch failed (e.g. cookie expired we'd redirect to the signing route:)
    --pure (set routeLens (SigninRoute Signin.initialState) state)
    fetch route (set usernameLens "alice" state)
  fetch (Settings s) state = do
    sr <- fetch (view SettingsRoute.routeLens s) s -- of type SettingsRoute
    pure (set routeLens (Settings sr) state)
  fetch rt s = do
    pure (set routeLens rt s)


initialState :: State
initialState = RouterState { currentRoute: EmptyRoute, username: "anonymous" }


routeLens :: LensP State RootRoutes
routeLens = lens (\(RouterState s) -> s.currentRoute) (\(RouterState s) x -> RouterState (s { currentRoute = x }))

usernameLens :: LensP State String
usernameLens = lens (\(RouterState s) -> s.username) (\(RouterState s) x -> RouterState (s { username = x }))


_SigninState :: PrismP State Signin.State
_SigninState = prism (\s -> set routeLens (SigninRoute s) initialState) \state ->
  case view routeLens state of
    SigninRoute s -> Right s
    _ -> Left state

_SigninAction :: PrismP Action Signin.Action
_SigninAction = prism SigninAction \action ->
  case action of
    SigninAction x -> Right x
    _ -> Left action

_SettingsState :: PrismP RootRoutes SettingsRoute.State
_SettingsState = prism Settings \route ->
  case route of
    Settings x -> Right x
    _ -> Left route

_SettingsAction :: PrismP Action SettingsRoute.Action
_SettingsAction = prism SettingsAction \action ->
  case action of
    SettingsAction x -> Right x
    _ -> Left action

_404State :: PrismP RootRoutes Unit
_404State = prism (const Route404) \route ->
  case route of
    Route404 -> Right unit
    _ -> Left route

_BrowseState :: PrismP RootRoutes Browse.State
_BrowseState = prism BrowseRoute \route ->
  case route of
    BrowseRoute x -> Right x
    _ -> Left route

_BrowseAction :: PrismP Action Browse.Action
_BrowseAction = prism BrowseAction \action ->
  case action of
    BrowseAction x -> Right x
    _ -> Left action


spec404 :: forall eff state props action. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) state props action
spec404 = T.simpleSpec T.defaultPerformAction render
  where
    render _ _ _ _ = [R.text "404"]

spec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) State props Action
spec = container $ handleActions $ fold
       [ T.split _SigninState (T.match _SigninAction Signin.spec)
       , T.focusState routeLens (T.split _SettingsState (T.match _SettingsAction  SettingsRoute.spec))
       , T.focusState routeLens (T.split _BrowseState (T.match _BrowseAction  Browse.spec))
       , T.focusState routeLens (T.split _404State spec404)
       ]
  where
    container = over T._render \render d p s c ->
      [ R.div [RP.className "container-fluid"] [R.text (view usernameLens s)]
      , R.div [RP.className "container-fluid"] (render d p s c)
      ]

    handleActions = over T._performAction \nestedPerformAction a p s k -> do
      nestedPerformAction a p s k
      handleAction a p s k

    -- TODO error handling when fetch fails
    handleAction action@(UpdateRoute r) p s k = do
      runAff (\err -> traceAnyM err >>= const (k id)) (\result -> k \s -> result) (fetch r s)
    handleAction _ _ _ _ = pure unit


-- The following is a hack to listen on route changes for the "root"
-- component that controls everything else. `dispatch` can be
-- extracted from the spec but takes a `this` pointer which is only
-- valid once we mounted a component.
componentDidMount :: forall props eff. (React.ReactThis props State -> Action -> T.EventHandler)
                     -> React.ComponentDidMount props State (console :: CONSOLE, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff)
componentDidMount dispatch this = do
    matches rootRoutes callback
  where
    callback :: forall eff2 refs. Maybe RootRoutes -> RootRoutes
                -> Eff ( props :: React.ReactProps
                       , state :: React.ReactState React.ReadWrite
                       , refs :: React.ReactRefs refs
                       , ajax :: AJ.AJAX
                       , cookie :: C.COOKIE | eff2) Unit
    callback _ rt = do
      maybeToken <- C.getCookie "auth-token" -- TODO this should go into the user fetching (which needs to check the token anyway)
      case maybeToken of
        -- force sign-in
        Nothing -> dispatch this (UpdateRoute (SigninRoute Signin.initialState))
        Just _ -> dispatch this (UpdateRoute rt)


component :: forall props. React.ReactClass props
component =
  let rspec = T.createReactSpec spec initialState
  in React.createClass
     ((_.spec rspec) { componentDidMount = (componentDidMount (_.dispatcher rspec)) })
