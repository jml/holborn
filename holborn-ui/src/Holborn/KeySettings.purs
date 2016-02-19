-- | Example module for how to do an UI element in our UI.

module Holborn.KeySettings where

import Prelude

import React as React
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T
import Network.HTTP.Affjax as AJ
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (runAff, Aff)
import Network.HTTP.Affjax (AJAX)
import Data.Foreign.Class (readJSON)
import Data.Either (Either(..))

import Holborn.Routing (Key(..))
import Unsafe.Coerce (unsafeCoerce)
import Network.HTTP.StatusCode (StatusCode(..))

-- The full internal state of this component. Components have state
-- and props. State is internal (e.g. component was loaded) and props
-- are external (e.g. colour of the button).
type State =
  { error :: String
  , loading :: Boolean
  , keys :: Array Key
  }

-- All possible state-modifying actions for this component.
data Action = AddKey

-- Initial State each time this component is inserted in the DOM.
initialState :: State
initialState =
  { error: ""
  , loading: false
  , keys: []
  }

type Props = {keys :: Array Key}

spec :: forall eff. T.Spec (ajax :: AJAX | eff) State Props Action
spec = T.simpleSpec performAction render
  where
    -- render is a react-ism. It renders the DOM fragment below and
    -- insert it efficiently by diffing the document-DOM with the
    -- fragment.
    render :: T.Render State Props Action
    render dispatch props s _ =
      [ R.h1 [] []
      , R.div [] [R.text if s.loading then "loading..." else ("loaded" ++ s.error)]
       , R.form []
        [ R.textarea [] []
        , R.button [RP.onClick \ev -> do
                       (unsafeCoerce ev).preventDefault
                       dispatch AddKey
                   ] [R.text "add new key" ]
        ]
      , R.div [] (map (\(Key key) -> (R.div [] [R.text key.title])) props.keys)
      ]

    -- performAction is a purescript-thermite callback. It takes an
    -- action and modifies the state by calling the callback k and
    -- passing it the modified state.
    performAction :: forall eff props. T.PerformAction (ajax :: AJAX | eff) State props Action
    performAction AddKey props state k = do
      k (state { loading = true })
      runAff (\err -> k state) k (addKey state)
      k (state { loading = false })

    addKey :: forall eff. State -> Aff (ajax :: AJAX | eff) State
    addKey state = do
      r <- AJ.post "http://127.0.0.1:8002/v1/user/keys" (toForeign {})
      return case r.status of
         StatusCode 200 -> case readJSON r.response of
             Left _ -> state { loading = false, error = "invalid json" }
             Right keys -> state { loading = false, keys = keys }
         _ -> state { loading = false, error = " [it broke]" }


component :: Props -> React.ReactElement
component props =
  React.createElement (T.createClass spec (initialState { keys = props.keys })) props []
