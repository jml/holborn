-- | Example module for how to do an UI element in our UI.

module Holborn.KeySettings where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff.Console (CONSOLE())
import Data.Either (Either(..))
import Data.Foreign.Class (IsForeign, readJSON, readProp)
import Debug.Trace (traceAnyM)
import qualified Data.Array as Array
import qualified Control.Monad.Eff.Exception as E
import qualified Network.HTTP.Affjax as AJ
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T


-- Counterpart to holborn-api:Holborn.JSON.Keys.Key
data Key =
  Key { key :: String
      , title :: String
      }


-- TODO. We're decoding JSON manully, we want to move to
-- auto-generated decoders ASAP (probably with servant-foreign).
instance objectIsForeign :: IsForeign Key where
  read value = do
    key <- readProp "key" value
    title <- readProp "title" value
    return $ Key { key: key, title: title }


-- The full internal state of this component. Components have state
-- and props. State is internal (e.g. component was loaded) and props
-- are external (e.g. colour of the button).
type State =
  { valid :: Boolean
  , loading :: Boolean
  , keys :: Array Key
  }

-- All possible state-modifying actions for this component.
data Action = AddKey | LoadData

-- Initial State each time this component is inserted in the DOM.
initialState :: State
initialState =
  { valid: false
  , loading: true
  , keys: []
  }


spec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX | eff) State props Action
spec = T.simpleSpec performAction render
  where
    -- render is a react-ism. It renders the DOM fragment below and
    -- insert it efficiently by diffing the document-DOM with the
    -- fragment.
    render :: T.Render State props Action
    render dispatch _ s _ =
      [ R.h1 [] [R.text ("Key settings. No keys: " ++ show (Array.length s.keys))]
      , R.div [] [R.text if s.loading then "loading..." else "loaded"]
      , R.form []
        [ R.textarea [] []
        , R.button [] [R.text "add new key" ]
        ]
      , R.div [] (map (\(Key key) -> (R.div [] [R.text key.title])) s.keys)
      ]

    -- performAction is a purescript-thermite callback. It takes an
    -- action and modifies the state by calling the callback k and
    -- passing it the modified state.
    performAction AddKey props state k = k $ state
    performAction LoadData props state k = do
      k (state { loading = true })
      runAff (\err -> traceAnyM err >>= \_ -> k $ state) k (loadKeys state)

    -- loadKeys is triggered by the LoadKeys action in
    -- componentDidMount. We use it to populate the UI from the
    -- server. The idea is to not show anything until we have fetched
    -- the data from the server.
    loadKeys state = do
      r <- AJ.get "http://127.0.0.1:8002/v1/users/tom/keys"
      return $ case readJSON r.response of
        Left err -> state -- TODO set or log error
        Right keys -> state { keys = keys, loading = false }


componentDidMount :: forall props state eff. (React.ReactThis props state -> Action -> T.EventHandler) -> R.ComponentDidMount props state (console :: CONSOLE | eff)
componentDidMount dispatch this = do
  -- We inject a LoadData actione every time this component is
  -- rendered in the react DOM.
  dispatch this LoadData


component :: forall props. props -> R.ReactElement
component props =
  let rspec = T.createReactSpec spec initialState
      cls = R.createClass ((_.spec rspec) { componentDidMount = (componentDidMount (_.dispatcher rspec))})
  in R.createElement cls props []
