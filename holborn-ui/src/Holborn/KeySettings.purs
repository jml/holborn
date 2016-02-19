-- | Example module for how to do an UI element in our UI.

module Holborn.KeySettings where

import Prelude

import React as React
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T

import Holborn.Routing (Key(..))

-- The full internal state of this component. Components have state
-- and props. State is internal (e.g. component was loaded) and props
-- are external (e.g. colour of the button).
type State =
  { valid :: Boolean
  , loading :: Boolean
  }

-- All possible state-modifying actions for this component.
data Action = AddKey

-- Initial State each time this component is inserted in the DOM.
initialState :: State
initialState =
  { valid: false
  , loading: true
  }

type Props = {keys :: Array Key}

spec :: forall eff. T.Spec eff State Props Action
spec = T.simpleSpec performAction render
  where
    -- render is a react-ism. It renders the DOM fragment below and
    -- insert it efficiently by diffing the document-DOM with the
    -- fragment.
    render :: T.Render State Props Action
    render dispatch props s _ =
      [ R.h1 [] []
      , R.div [] [R.text if s.loading then "loading..." else "loaded"]
      , R.form []
        [ R.textarea [] []
        , R.button [] [R.text "add new key" ]
        ]
      , R.div [] (map (\(Key key) -> (R.div [] [R.text key.title])) props.keys)
      ]

    -- performAction is a purescript-thermite callback. It takes an
    -- action and modifies the state by calling the callback k and
    -- passing it the modified state.
    performAction AddKey props state k = k $ state


component :: Props -> React.ReactElement
component props =
  React.createElement (T.createClass spec initialState) props []
