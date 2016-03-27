module Components.Router where

import Prelude
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception as E
import Control.Monad.Eff.Class (liftEff)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Lens(PrismP, prism, over, lens, LensP, view, set)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax as AJ
import React as React
import React.DOM as R
import React.DOM.Props as RP
import Unsafe.Coerce (unsafeCoerce)
import Data.Argonaut.Decode (decodeJson)

import Text.Parsing.Simple (Parser, string)
import Standalone.Router.Dispatch (matches, navigate)
import Thermite as T
import Web.Cookies as C

import Holborn.Browse as Browse
import Holborn.Fetchable (class Fetchable, fetch)
import Holborn.SettingsRoute as SettingsRoute
import Holborn.Signin as Signin
import Holborn.Config (makeUrl)
import Holborn.Auth as Auth
import Debug.Trace
import Holborn.ManualEncoding.Profile as ManualCodingProfile


data UserMeta = SignedIn { username :: String, about :: String } | NotLoaded | Anonymous

data State = RouterState { currentRoute :: RootRoutes, _userMeta :: UserMeta }


data RootRoutes =
    EmptyRoute
  | Route404
  | SigninRoute Signin.State
  | Settings SettingsRoute.State -- TODO fix inconsitent naming
  | BrowseRoute Browse.State

-- TODO tom: Routes should really be "invertible" so I can create a
-- KeySettings route string from the value.
rootRoutes :: Parser RootRoutes
rootRoutes =
  string "/" *>
    ( string "settings/" *> (map Settings SettingsRoute.settingsRoutes)
      <|> map BrowseRoute Browse.browseRoutes
      <|> pure Route404
    )


data Action =
  UpdateRoute RootRoutes
  | SigninAction Signin.Action
  | SettingsAction SettingsRoute.Action
  | BrowseAction Browse.Action


-- TODO: As jml observed fetching (and our entire app) is essentially
-- a state monad but it's a bit unclear how to fit that observation
-- into real code.
instance fetchRootRoutes :: Fetchable RootRoutes State where
  fetch route state@(RouterState { _userMeta = NotLoaded }) = do
      maybeToken <- liftEff (C.getCookie "auth-token")
      newState <- case maybeToken of
        Nothing -> pure (set userMeta Anonymous state)
        Just _ -> do
          r <- Auth.get (makeUrl "/v1/user")
          case decodeJson r.response of
            Left err ->
              pure (set routeLens (SigninRoute Signin.initialState) state)
            Right (ManualCodingProfile.Profile json) ->
              pure (set userMeta (SignedIn {username: json.username, about: json.about}) state)

      fetch route newState

  fetch (Settings s) state = do
      sr <- fetch (view SettingsRoute.routeLens s) s -- of type SettingsRoute
      pure (set routeLens (Settings sr) state)

  -- Slightly different to settings: If we are already in a browse
  -- route then recycle existing state (browseState).
  fetch (BrowseRoute s) state = do
    newState <- case view routeLens state of
      BrowseRoute browseState -> fetch (view Browse.routeLens s) browseState
      _ -> fetch (view Browse.routeLens s) s
    pure (set routeLens (BrowseRoute newState) state)

  fetch rt s = do
      pure (set routeLens rt s)


initialState :: State
initialState = RouterState { currentRoute: EmptyRoute, _userMeta: NotLoaded }


routeLens :: LensP State RootRoutes
routeLens = lens (\(RouterState s) -> s.currentRoute) (\(RouterState s) x -> RouterState (s { currentRoute = x }))

userMeta :: LensP State UserMeta
userMeta = lens (\(RouterState s) -> s._userMeta) (\(RouterState s) x -> RouterState (s { _userMeta = x }))


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
    container = over T._render \render d p s c -> case view userMeta s of
      NotLoaded -> [R.text "loading UI ..."]
      Anonymous ->
        [ R.div [RP.className "container-fluid", RP.onClick handleLinks] [R.text "sign in here ..."]
        , R.div [RP.className "container-fluid", RP.onClick handleLinks] (render d p s c)
        ]
      SignedIn { username, about } ->
        [ R.div [RP.className "container-fluid", RP.onClick handleLinks] [R.text username, R.text about]
        , R.div [RP.className "container-fluid", RP.onClick handleLinks] (render d p s c)
        ]


    -- Override link navigation to use pushState instead of the
    -- browser following the link.
    -- TODO: add escape-hatch, e.g. `data-external=true` attribute or
    -- where the path is non-local
    handleLinks ev = do
      case (unsafeCoerce ev).target.nodeName of
        "A" -> do
          (unsafeCoerce ev).preventDefault
          navigate ((unsafeCoerce ev).target.pathname)
        _ -> pure unit

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
    callback :: forall eff2 refs. RootRoutes
                -> Eff ( props :: React.ReactProps
                       , state :: React.ReactState React.ReadWrite
                       , refs :: React.ReactRefs refs
                       , ajax :: AJ.AJAX
                       , cookie :: C.COOKIE | eff2) Unit
    callback rt = dispatch this (UpdateRoute rt)


component :: forall props. React.ReactClass props
component =
  let rspec = T.createReactSpec spec initialState
  in React.createClass
     ((_.spec rspec) { componentDidMount = (componentDidMount (_.dispatcher rspec)) })
