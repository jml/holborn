module Holborn.Router where

import Prelude
import Control.Alt ((<|>))
import Control.Apply ((*>))
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
import Holborn.SettingsRoute as Settings
import Holborn.Signin as Signin
import Holborn.Config (makeUrl)
import Holborn.Auth as Auth
import Debug.Trace (spy)
import Holborn.ManualEncoding.Profile as ManualCodingProfile
import Holborn.DomHelpers (scroll)
import Network.HTTP.StatusCode (StatusCode(..))


data UserMeta = SignedIn { username :: String, about :: String } | NotLoaded | Anonymous

data State = RouterState
    { currentRoute :: RootRoutes
    , _userMeta :: UserMeta
    , _burgerOpen :: Boolean
    }


data RootRoutes =
    EmptyRoute
  | Route404
  | SigninRoute Signin.State
  | SettingsRoute Settings.State -- TODO fix inconsitent naming
  | BrowseRoute Browse.State

-- TODO tom: Routes should really be "invertible" so I can create a
-- KeySettings route string from the value.
rootRoutes :: Parser String RootRoutes
rootRoutes =
  string "/" *>
    ( string "settings/" *> (map SettingsRoute Settings.settingsRoutes)
      <|> string "signin" *> pure (SigninRoute Signin.initialState)
      <|> map BrowseRoute Browse.browseRoutes
      <|> pure Route404
    )


data Action =
  UpdateRoute RootRoutes
  | SigninAction Signin.Action
  | SettingsAction Settings.Action
  | BrowseAction Browse.Action
  | BurgerMenuToggle


-- TODO: As jml observed fetching (and our entire app) is essentially
-- a state monad but it's a bit unclear how to fit that observation
-- into real code.
instance fetchRootRoutes :: Fetchable RootRoutes State where
  fetch route state@(RouterState { _userMeta: NotLoaded }) = do
      maybeToken <- liftEff (C.getCookie "auth-token")
      newState <- case maybeToken of
        Nothing -> pure (set userMeta Anonymous state)
        Just _ -> do
          r <- Auth.get (makeUrl "/v1/user")

          -- Delete cookie if we get unauthorized for this cookie
          -- (might be expired or maybe never was valid).
          case r.status of
            StatusCode 401 -> liftEff (C.deleteCookie "auth-token")
            _ -> pure unit

          case decodeJson r.response of
            Left err ->
              pure (set routeLens (SigninRoute Signin.initialState) state)

            Right (ManualCodingProfile.Profile json) ->
              pure (set userMeta (SignedIn {username: json.username, about: json.about}) state)

      fetch route newState

  fetch (SettingsRoute s) state = do
      sr <- fetch (view Settings.routeLens s) s -- of type SettingsRoute
      pure (set routeLens (SettingsRoute sr) state)

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
initialState = RouterState { currentRoute: EmptyRoute, _userMeta: NotLoaded, _burgerOpen: false }


routeLens :: LensP State RootRoutes
routeLens = lens (\(RouterState s) -> s.currentRoute) (\(RouterState s) x -> RouterState (s { currentRoute = x }))

burgerOpen :: LensP State Boolean
burgerOpen = lens (\(RouterState s) -> s._burgerOpen) (\(RouterState s) x -> RouterState (s { _burgerOpen = x }))

userMeta :: LensP State UserMeta
userMeta = lens (\(RouterState s) -> s._userMeta) (\(RouterState s) x -> RouterState (s { _userMeta = x }))


_SigninState :: PrismP RootRoutes Signin.State
_SigninState = prism SigninRoute \route ->
  case route of
    SigninRoute s -> Right s
    _ -> Left route

_SigninAction :: PrismP Action Signin.Action
_SigninAction = prism SigninAction \action ->
  case action of
    SigninAction x -> Right x
    _ -> Left action

_SettingsState :: PrismP RootRoutes Settings.State
_SettingsState = prism SettingsRoute \route ->
  case route of
    SettingsRoute x -> Right x
    _ -> Left route

_SettingsAction :: PrismP Action Settings.Action
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
       [ T.focusState routeLens (T.split _SigninState (T.match _SigninAction Signin.spec))
       , T.focusState routeLens (T.split _SettingsState (T.match _SettingsAction  Settings.spec))
       , T.focusState routeLens (T.split _BrowseState (T.match _BrowseAction  Browse.spec))
       , T.focusState routeLens (T.split _404State spec404)
       ]
  where
    container = over T._render \render d p s c -> case view userMeta s of
      NotLoaded -> [R.text "loading UI ..."]

      Anonymous ->
        [ R.div [RP.onClick handleLinks] [R.a [RP.href "/signin"] [R.text "sign in here ..."]]
        , R.div [RP.onClick handleLinks] (render d p s c)
        ]

      SignedIn { username, about } ->
        -- TODO: for reasons I don't understand the onClick handler on
        -- the top-level div is never called so I need to add more
        -- specific onClick handlers
        [ R.div [RP.onClick handleLinks]
          [ R.header []
            [ R.div [RP.onClick (burgerMenuToggle d), RP.className "burger" ] [ R.text "=" ]
            , R.div [RP.className "context" ] [ R.text (contextLabel s) ]
            , R.div [RP.className "search" ] [ R.input [RP.placeholder "Search"] [] ]
            , R.div [RP.className "pad" ] []
            , R.div [RP.className "me" ] [ R.text "ME" ]
            ]
          , R.div []
            [ R.text username
            , R.text about
            , R.a [RP.href "/src/werkzeug"] [R.text "werkzeug"]
            ]
          ]
        , R.section
          [ RP.className if view burgerOpen s then "content burger-menu-open" else "content", RP.onClick handleLinks] (render d p s c)
        , burgerMenu s
        ]

    burgerMenu s =
      R.div [RP.className if spy (view burgerOpen s) then "burger-menu open" else "burger-menu", RP.onClick handleLinks]
      [ R.ul []
        [ R.li [] [R.text "Dashboard"]
        , R.li [] [R.text "My Projects"]
        , R.li [] [R.text "Candidates"]
        , R.li [] [R.a [RP.href "/settings/ssh-keys" ] [R.text "Settings"]]
        ]
      ]

    contextLabel s = case view routeLens s of
      EmptyRoute -> "loading.."
      Route404 -> "404"
      SigninRoute _ -> "Sign In"
      SettingsRoute _ -> "Settings"
      BrowseRoute _ -> "Browse"

    burgerMenuToggle dispatch ev = dispatch BurgerMenuToggle

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

    handleActions = over T._performAction \nestedPerformAction a p s -> do
      nestedPerformAction a p s
      handleAction a p s

    -- TODO error handling when fetch fails
    handleAction BurgerMenuToggle p s = void $ T.cotransform (\s -> over burgerOpen not s)

    handleAction action@(UpdateRoute r) p s = void $ T.cotransform id

    handleAction _ _ _ = void $ T.cotransform id

--      runAff (\err -> traceAnyM err >>= const (k id)) (\result -> k \s -> result) (fetch r s)
--    handleAction _ _ _ _ = pure unit


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


componentDidUpdate :: forall props eff state. React.ComponentDidUpdate props state eff
componentDidUpdate this props state = scroll 0 0


component :: forall props. React.ReactClass props
component =
  let rspec = T.createReactSpec spec initialState
  in React.createClass
     ((_.spec rspec) { componentDidMount = (componentDidMount (_.dispatcher rspec))
                     , componentDidUpdate = componentDidUpdate
                     })
