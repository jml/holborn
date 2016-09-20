module Holborn.Router where

import Prelude
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception as E
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Lens(PrismP, prism, over, lens, LensP, view, set)
import Network.HTTP.Affjax as AJ
import React as React
import React.DOM as R
import React.DOM.Props as RP
import Unsafe.Coerce (unsafeCoerce)
import Data.Argonaut.Decode (decodeJson)
import Standalone.Router.Dispatch (Navigate, matches, navigate, navigateA)

import DOM (DOM)

import Text.Parsing.Simple (Parser, string, eof, tail)
import Thermite as T
import Standalone.Thermite as T

import Holborn.Browse as Browse
import Holborn.Fetchable (class Fetchable, fetch)
import Holborn.SettingsRoute as Settings
import Holborn.CreateAccount as CreateAccount
import Holborn.CreateRepository as CreateRepository

import Holborn.Config (makeUrl)
import Holborn.Auth as Auth
import Holborn.ManualEncoding.Profile as ManualCodingProfile
import Holborn.DomHelpers (scroll)
import Network.HTTP.StatusCode (StatusCode(..))
import Control.Monad.Trans (lift)
import Debug.Trace


-- | UserMeta records the state of the visiting user from the
-- database.
data UserMeta =
    NotLoaded -- | we need to fetch from the server to determine the
              -- user's state before rendering otherwise there could
              -- be some flickering from Anonymous to SignedIn
  | Anonymous
  | SignedIn { username :: String, about :: String }


data State = RouterState
    { currentRoute :: RootRoutes
    , _userMeta :: UserMeta
    , _burgerOpen :: Boolean
    }


data RootRoutes =
    NotLoadedRoute
  | Route404
  | LandingPageRoute
  | SettingsRoute Settings.State
  | BrowseRoute Browse.State
  | CreateAccountRoute CreateAccount.State
  | CreateRepositoryRoute CreateRepository.State


-- TODO tom: Routes should really be "invertible" so I can create a
-- KeySettings route string from the value.
rootRoutes :: Parser String RootRoutes
rootRoutes =
  string "/" *>
    ( string "settings/" *> (map SettingsRoute Settings.settingsRoutes)
      <|> string "create-account" *> pure (CreateAccountRoute CreateAccount.initialState)
      <|> string "create-repository" *> pure (CreateRepositoryRoute CreateRepository.initialState)
      <|> string "code/" *> map BrowseRoute Browse.browseRoutes
      <|> pure LandingPageRoute <* eof
      <|> pure Route404 <* tail
    )


data Action =
  UpdateRoute RootRoutes
  | SearchRepo Browse.Owner Browse.Repo String
  | SearchOwner Browse.Owner String
  | SettingsAction Settings.Action
  | BrowseAction Browse.Action
  | CreateAccountAction CreateAccount.Action
  | CreateRepositoryAction CreateRepository.Action
  | BurgerMenuToggle


-- TODO: As jml observed fetching (and our entire app) is essentially
-- a state monad but it's a bit unclear how to fit that observation
-- into real code.
instance fetchRootRoutes :: Fetchable RootRoutes State where
  fetch route state@(RouterState { _userMeta: NotLoaded }) = do
    r <- Auth.get (makeUrl "/v1/user")

    case r.status of
      StatusCode 401 -> fetch route (set userMeta Anonymous state)

      -- 418 is the no-user-account-yet error. It always forces a
      -- redirect to the CreateAccount route.
      StatusCode 418 -> do
        navigateA "/create-account"
        fetch (CreateAccountRoute CreateAccount.initialState) (set userMeta Anonymous state)

      -- TODO JSON parser errors should show up as a generic error,
      -- e.g. by having a state that shows a message to the
      -- user. Returning "pure state" is the worst because it ignores
      -- the parser error.
      _ -> case decodeJson r.response of
        Left err -> do
          fetch route state
        Right (ManualCodingProfile.Profile json) ->
          fetch route (set userMeta (SignedIn {username: json.username, about: json.about}) state)

  -- Catch the redirect from account creation to dashboard route. TODO
  -- Route404 is just a placeholder for the dashboard route.
  fetch Route404 state@(RouterState { currentRoute: (CreateAccountRoute _)}) = do
    fetch NotLoadedRoute (set userMeta NotLoaded state)

  fetch (SettingsRoute s) state = do
    sr <- fetch (view Settings.routeLens s) s -- of type SettingsRoute
    pure (set routeLens (SettingsRoute sr) state)

  fetch (CreateRepositoryRoute s) state = do
    -- TODO this fetch branch should probably go into CreateRepository.purs
    r <- Auth.get (makeUrl "/v1/user/repository-owner-candidates")
    case Auth.handleStatus r of
        Auth.OK r' -> pure $ set routeLens (CreateRepositoryRoute (CreateRepository.fetchUpdate r' s)) state
        Auth.FormError (_ :: Unit) -> pure state
        x -> traceAnyM x *> pure state

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
initialState = RouterState { currentRoute: NotLoadedRoute, _userMeta: NotLoaded, _burgerOpen: false }

routeLens :: LensP State RootRoutes
routeLens = lens (\(RouterState s) -> s.currentRoute) (\(RouterState s) x -> RouterState (s { currentRoute = x }))

burgerOpen :: LensP State Boolean
burgerOpen = lens (\(RouterState s) -> s._burgerOpen) (\(RouterState s) x -> RouterState (s { _burgerOpen = x }))

userMeta :: LensP State UserMeta
userMeta = lens (\(RouterState s) -> s._userMeta) (\(RouterState s) x -> RouterState (s { _userMeta = x }))


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

_LandingPageState :: PrismP RootRoutes Unit
_LandingPageState = prism (const LandingPageRoute) \route ->
  case route of
    LandingPageRoute -> Right unit
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


_CreateAccountState :: PrismP RootRoutes CreateAccount.State
_CreateAccountState = prism CreateAccountRoute \route ->
  case route of
    CreateAccountRoute x -> Right x
    _ -> Left route

_CreateAccountAction :: PrismP Action CreateAccount.Action
_CreateAccountAction = prism CreateAccountAction \action ->
  case action of
    CreateAccountAction x -> Right x
    _ -> Left action

_CreateRepositoryState :: PrismP RootRoutes CreateRepository.State
_CreateRepositoryState = prism CreateRepositoryRoute \route ->
  case route of
    CreateRepositoryRoute x -> Right x
    _ -> Left route

_CreateRepositoryAction :: PrismP Action CreateRepository.Action
_CreateRepositoryAction = prism CreateRepositoryAction \action ->
  case action of
    CreateRepositoryAction x -> Right x
    _ -> Left action


spec404 :: forall eff state props action. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX | eff) state props action
spec404 = T.simpleSpec T.defaultPerformAction render
  where
    render _ _ _ _ = [R.text "404"]


landingPage :: forall eff state props action. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX | eff) state props action
landingPage = T.simpleSpec T.defaultPerformAction render
  where
    render _ _ _ _ =
      [R.ul []
       [ R.li [] [R.a [RP.href "/create-repository"] [R.text "Create a new repository"]]
       ]
      ]


searchSpec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, navigate :: Navigate | eff) State props Action
searchSpec = T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render State props Action
    render dispatch _ state _ = [ R.div [RP.className "search" ] (renderSearch state) ]

    renderSearch :: _
    renderSearch s = case view routeLens s of
      -- TODO fancy search box that allows removing "repo" search
      -- TODO autocomplete
      BrowseRoute state ->
        [ R.input [RP.placeholder ("Search in repository " <> Browse.searchLink state "")] []
        ]
      _ -> [ R.input [RP.placeholder "Search"] [] ]


containerSpec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, dom :: DOM, navigate :: Navigate | eff) State props Action
containerSpec = T.simpleSpec handleAction render
  where
    render d p s children = case view userMeta s of
      NotLoaded -> [R.text "loading UI ..."]

      Anonymous ->
        [ pageHeaderSignedOut d p s children
          -- /signing is a *real* link that doesn't go through pushState,
          -- the frontend terminator will capture this and redirect to dex.
        , R.div [] [R.a [RP.href "/signin"] [R.text "sign in here ..."]]
        , R.section
          [ RP.className "content", RP.onClick handleLinks] children
        ]

      SignedIn { username, about } ->
        -- TODO: for reasons I don't understand the onClick handler on
        -- the top-level div is never called so I need to add more
        -- specific onClick handlers
        [ R.div [RP.onClick handleLinks]
          [ pageHeaderSignedIn username d p s children
          , R.div []
            [ R.text username
            , R.text about
            , R.a [RP.href "/src/werkzeug"] [R.text "werkzeug"]
            ]
          ]
        , R.section
          [ RP.className if view burgerOpen s then "content burger-menu-open" else "content", RP.onClick handleLinks] children
        , burgerMenu s
        ]

    pageHeaderSignedIn :: String -> (Action -> T.EventHandler) -> props -> State -> Array React.ReactElement -> React.ReactElement
    pageHeaderSignedIn username d _ s c = R.header []
            ([ R.div [RP.onClick (burgerMenuToggle d), RP.className "burger" ] [ R.text "=" ]
             , R.div [RP.className "context" ] [ R.text (contextLabel s) ]
             ] <> (view T._render searchSpec d {} s []) <>
             [ R.div [RP.className "pad" ] []
             , R.div [RP.className "me" ] [ R.text username ]
             ])

    pageHeaderSignedOut :: (Action -> T.EventHandler) -> props -> State -> Array React.ReactElement -> React.ReactElement
    pageHeaderSignedOut d _ s c = R.header []
            ([ R.div [RP.className "context" ] [ R.text (contextLabel s) ]
             ] <> (view T._render searchSpec d {} s []) <>
             [ R.div [RP.className "pad" ] []
             , R.div [RP.className "me" ] [ R.a [RP.href "/signin"] [R.text "Signin"] ]
             ])

    burgerMenu s =
      R.div [RP.className if view burgerOpen s then "burger-menu open" else "burger-menu", RP.onClick handleLinks]
      [ R.ul []
        [ R.li [] [R.text "Dashboard"]
        , R.li [] [R.text "My Projects"]
        , R.li [] [R.text "Candidates"]
        , R.li [] [R.a [RP.href "/settings/ssh-keys" ] [R.text "Settings"]]
        ]
      ]

    -- The `contextLabel` is the little message in the top left
    -- telling us where we are. Not sure whether we want to keep this
    -- or not but useful for development ATM.
    contextLabel s = case view routeLens s of
      NotLoadedRoute -> "loading.."
      Route404 -> "404"
      SettingsRoute _ -> "Settings"
      BrowseRoute _ -> "Browse"
      CreateAccountRoute _ -> "Last step!"
      LandingPageRoute -> "Home"
      _ -> "" -- don't show a label for all other cases

    burgerMenuToggle dispatch ev = dispatch BurgerMenuToggle

    -- Override link navigation to use pushState instead of the
    -- browser following the link.
    --
    -- TODO: add escape-hatch, e.g. `data-external=true` attribute or
    -- where the path is non-local. This allows us to link
    -- e.g. off-site.
    handleLinks ev = do
      case (unsafeCoerce ev).target.nodeName of
        "A" -> do
          (unsafeCoerce ev).preventDefault
          navigate ((unsafeCoerce ev).target.pathname)
        _ -> pure unit

    handleAction BurgerMenuToggle p s = void $ T.cotransform (\s -> over burgerOpen not s)

    handleAction action@(UpdateRoute r) p s = do
      s' <- lift (fetch r s)
      void (T.cotransform (const s'))

    handleAction (SearchRepo owner repo q) p s = do
      lift (navigateA ("/" <> owner <> "/" <> repo <> "q=" <> q))

    handleAction _ _ _ = void (T.cotransform id)


-- | spec is a Thermite-ism. It controls the flow of actions and
-- dispatches rendering code. Based on the state of the route
-- component we both focus and split into subcomponents (and the
-- subcompontents may do the same).
spec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, dom :: DOM, navigate :: Navigate | eff) State props Action
spec = T.nestSpec containerSpec $ foldMap (T.focusState routeLens)
       [ T.split _SettingsState (T.match _SettingsAction Settings.spec)
       , T.split _BrowseState (T.match _BrowseAction Browse.spec)
       , T.split _CreateAccountState (T.match _CreateAccountAction CreateAccount.spec)
       , T.split _CreateRepositoryState (T.match _CreateRepositoryAction CreateRepository.spec)
       , T.split _404State spec404
       , T.split _LandingPageState landingPage
       ]

-- The following is a hack to listen on route changes for the "root"
-- component that controls everything else. `dispatch` could be
-- extracted from the spec but takes a `this` pointer which is only
-- valid once we mounted a component.
componentDidMount :: forall props eff. (React.ReactThis props State -> Action -> T.EventHandler)
                     -> React.ComponentDidMount props State (console :: CONSOLE, ajax :: AJ.AJAX, dom :: DOM | eff)
componentDidMount dispatch this = do
    matches rootRoutes callback
  where
    callback :: forall eff2 refs. RootRoutes
                -> Eff ( props :: React.ReactProps
                       , state :: React.ReactState React.ReadWrite
                       , refs :: React.ReactRefs refs
                       , ajax :: AJ.AJAX| eff2) Unit
    callback rt = dispatch this (UpdateRoute rt)


componentDidUpdate :: forall props eff state. React.ComponentDidUpdate props state eff
componentDidUpdate this props state = do
  -- TODO shall we allow our app to hook into state changes?
  -- That would e.g. allow us to catch the CreateAccount finished state change.
  scroll 0 0


component :: forall props. React.ReactClass props
component =
  let rspec = T.createReactSpec spec initialState
  in React.createClass
     ((_.spec rspec) { componentDidMount = (componentDidMount (_.dispatcher rspec))
                     , componentDidUpdate = componentDidUpdate
                     })
