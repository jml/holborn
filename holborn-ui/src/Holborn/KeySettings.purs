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
import Data.Either (Either(..))
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.List (List(..), (:), toUnfoldable)
import Data.Lens (view, set)

import Holborn.ManualEncoding.Keys (Key(..), AddKeyData(..), title, key)
import Unsafe.Coerce (unsafeCoerce)
import Network.HTTP.StatusCode (StatusCode(..))

import Debug.Trace

-- The full internal state of this component. Components have state
-- and props. State is internal (e.g. component was loaded) and props
-- are external (e.g. colour of the button).
type State =
  { error :: String
  , loading :: Boolean
  , keys :: List Key
  , addKeyData :: AddKeyData
  }

-- All possible state-modifying actions for this component.
data Action = AddKey | UpdateKeyData AddKeyData

emptyAddKeyData = AddKeyData { key: "", title: "" }

-- Initial State each time this component is inserted in the DOM.
initialState :: State
initialState =
  { error: ""
  , loading: false
  , keys: Nil
  , addKeyData: emptyAddKeyData
  }

type Props = {keys :: List Key}

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
       , R.form [RP.onSubmit onSubmit]
        [ R.input [ RP.value (view title s.addKeyData)
                  , RP.onChange \ev -> dispatch (UpdateKeyData (set title (unsafeCoerce ev).target.value s.addKeyData))
                  ] []
        , R.textarea [ RP.value (view key s.addKeyData)
                     , RP.onChange \ev -> dispatch (UpdateKeyData (set key (unsafeCoerce ev).target.value s.addKeyData))
                     ] []
        , R.button [RP.disabled s.loading] [R.text "add new key"]
        ]
      , R.div [] keyArray
      ]
      where
        onSubmit ev = do
          (unsafeCoerce ev).preventDefault
          dispatch AddKey
        keyArray = toUnfoldable (map (\(Key key) -> (R.div [] [R.text key.title])) s.keys)

    -- performAction is a purescript-thermite callback. It takes an
    -- action and modifies the state by calling the callback k and
    -- passing it the modified state.
    performAction :: forall eff props. T.PerformAction (ajax :: AJAX | eff) State props Action
    performAction (UpdateKeyData x) props state k = k $ state { addKeyData = x }
    performAction AddKey props state k = do
      k (state { loading = true })
      runAff (\err -> k (state { error = show err})) k (addKey state)

    -- Server fetching can go in many ways and we'll need to reflect
    -- errors in the state and allow users to move on from there
    -- (e.g. re-enable buttons for retry, display actual invalid input
    -- errors etc).
    addKey :: forall eff. State -> Aff (ajax :: AJAX | eff) State
    addKey state = do
      r <- AJ.post "http://127.0.0.1:8002/v1/user/keys" (encodeJson state.addKeyData)
      return case r.status of
         StatusCode 201 -> case decodeJson r.response of
             Left err -> state { loading = false, error = "invalid json: " ++ err }
             Right key -> state { loading = false, keys = key : state.keys, error = " OK", addKeyData = emptyAddKeyData}

         -- Note that by calling `decodeJson` in the 201 branch the
         -- type inference decided that the response must be JSON so
         -- we need to send back valid JSON in the 400 case as well.
         StatusCode 400 -> state { loading = false, error = " - 400" }
         _ -> state { loading = false, error = " [it broke]" }


component :: Props -> React.ReactElement
component props =
  React.createElement (T.createClass spec (initialState { keys = props.keys })) props []
