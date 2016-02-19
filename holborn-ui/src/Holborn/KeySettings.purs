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


import Holborn.ManualEncoding.Keys (Key(..), AddKeyData(..))
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
  }

-- All possible state-modifying actions for this component.
data Action = AddKey

-- Initial State each time this component is inserted in the DOM.
initialState :: State
initialState =
  { error: ""
  , loading: false
  , keys: Nil
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
       , R.form []
        [ R.textarea [] []
        , R.button [RP.onClick \ev -> do
                       (unsafeCoerce ev).preventDefault
                       dispatch AddKey
                   ] [R.text "add new key" ]
        ]
      , R.div [] keyArray
      ]
      where
        keyArray = (toUnfoldable (map (\(Key key) -> (R.div [] [R.text key.title])) s.keys))

    -- performAction is a purescript-thermite callback. It takes an
    -- action and modifies the state by calling the callback k and
    -- passing it the modified state.
    performAction :: forall eff props. T.PerformAction (ajax :: AJAX | eff) State props Action
    performAction AddKey props state k = do
      k (state { loading = true })
      runAff (\err -> k state) k (addKey state)

    -- Server fetching can go in many ways and we'll need to reflect
    -- errors in the state and allow users to move on from there
    -- (e.g. re-enable buttons for retry, display actual invalid input
    -- errors etc).
    addKey :: forall eff. State -> Aff (ajax :: AJAX | eff) State
    addKey state = do
      r <- AJ.post "http://127.0.0.1:8002/v1/user/keys" (encodeJson (AddKeyData{ key: "", title: "title" }))
      return case r.status of
         StatusCode 201 -> case decodeJson r.response of
             Left err -> state { loading = false, error = "invalid json: " ++ err }
             Right key -> state { loading = false, keys = key : state.keys, error = " OK" }
         _ -> state { loading = false, error = " [it broke]" }

component :: Props -> React.ReactElement
component props =
  React.createElement (T.createClass spec (initialState { keys = props.keys })) props []
