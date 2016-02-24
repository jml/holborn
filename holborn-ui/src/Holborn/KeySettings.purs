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
import Data.Maybe (Maybe(..))

import Holborn.ManualEncoding.Keys (Key(..), AddKeyData(..), AddKeyDataError(..), title, key)
import Unsafe.Coerce (unsafeCoerce)
import Network.HTTP.StatusCode (StatusCode(..))
import Data.Lens (LensP)

import Debug.Trace (traceAnyM)


-- The full internal state of this component. Components have state
-- and props. State is internal (e.g. component was loaded) and props
-- are external (e.g. colour of the button).
type State =
  { error :: AddKeyDataError
  , loading :: Boolean
  , keys :: List Key
  , addKeyData :: AddKeyData
  }

-- All possible state-modifying actions for this component.
data Action = AddKey | UpdateKeyData AddKeyData

emptyAddKeyData = AddKeyData { key: "", title: "" }

emptyAddKeyDataError = AddKeyDataError { global: Nothing, key: Nothing, title: Nothing }
networkAddKeyDataError msg =  AddKeyDataError { global: Just msg, key: Nothing, title: Nothing }

-- Initial State each time this component is inserted in the DOM.
initialState :: State
initialState =
  { error: emptyAddKeyDataError
  , loading: false
  , keys: Nil
  , addKeyData: emptyAddKeyData
  }

type Props = {keys :: List Key}


-- We need to unsafeCoerce event because the purescript-react bindings
-- aren't exposing preventDefault.
preventDefault :: forall m. React.Event -> m
preventDefault ev = (unsafeCoerce ev).preventDefault

-- We need unsafeCoerce because purescript-react bindings arent
-- exposing target.value yet.
fieldUpdater :: forall a s x. LensP s x -> React.Event -> s -> s
fieldUpdater setter ev state = set setter (unsafeCoerce ev).target.value state


-- TODO: Tom is a bit unsure about the best way to organise the render
-- functions. I think that with more forms some common elements will
-- emerge and we can consolidate then.
labeled label err formEl = case err of
  Nothing -> R.div [ RP.className "form-group" ] [ R.label [] [R.text label],  formEl ]
  Just msg -> R.div [ RP.className "form-group has-error" ] [ R.label [] [R.text (label ++ " - " ++ msg)],  formEl ]

renderGlobalError err = case err of
  Nothing -> R.text ""
  Just msg -> R.div [] [R.text msg]


spec :: forall eff. T.Spec (ajax :: AJAX | eff) State Props Action
spec = T.simpleSpec performAction render
  where
    -- render is a react-ism. It renders the DOM fragment below and
    -- insert it efficiently by diffing the document-DOM with the
    -- fragment.
    render :: T.Render State Props Action
    render dispatch props ({ addKeyData, error: AddKeyDataError err, loading, keys }) _ =
      [ R.h1 [] [R.text "Manage SSH keys"]
      , renderGlobalError err.global
       , R.form [RP.onSubmit onSubmit]
        [ labeled "Key name" err.title $ R.input [ RP.value (view title addKeyData)
                  , RP.className "form-control"
                  , RP.onChange \ev -> dispatch (UpdateKeyData (fieldUpdater title ev addKeyData))
                  ] []
        , labeled "Key" err.key $ R.textarea [ RP.value (view key addKeyData)
                     , RP.onChange \ev -> dispatch (UpdateKeyData (fieldUpdater key ev addKeyData))
                     , RP.className "form-control"
                     ] []
        , R.button [RP.disabled loading, RP.className "btn btn-default"] [R.text if loading then "Adding key ..." else "Add new key"]
        ]
      , R.div [] keyArray
      ]
      where
        onSubmit ev = do
          (unsafeCoerce ev).preventDefault
          dispatch AddKey
        keyArray = toUnfoldable (map (\(Key key) -> (R.div [] [R.text key.title])) keys)

    -- performAction is a purescript-thermite callback. It takes an
    -- action and modifies the state by calling the callback k and
    -- passing it the modified state.
    performAction :: forall eff props. T.PerformAction (ajax :: AJAX | eff) State props Action
    performAction (UpdateKeyData x) props state k = k $ state { addKeyData = x }
    performAction AddKey props state k = do
      k (state { loading = true })
      runAff (\err -> traceAnyM err >>= \_ -> k $ state { error = networkAddKeyDataError "No network connection. Please try again later"}) k (addKey state)

    -- Server fetching can go in many ways and we'll need to reflect
    -- errors in the state and allow users to move on from there
    -- (e.g. re-enable buttons for retry, display actual invalid input
    -- errors etc).
    addKey :: forall eff. State -> Aff (ajax :: AJAX | eff) State
    addKey state = do
      r <- AJ.post "http://127.0.0.1:8002/v1/user/keys" (encodeJson state.addKeyData)
      return case r.status of
         StatusCode 201 -> case decodeJson r.response of
             Left err -> state { loading = false, error = networkAddKeyDataError "Something unexpeced broke." }
             Right key -> state { loading = false, keys = key : state.keys, addKeyData = emptyAddKeyData}

         -- Note that by calling `decodeJson` in the 201 branch the
         -- type inference decided that the response must be JSON so
         -- we need to send back valid JSON in the 400 case as well.
         StatusCode 400 -> case decodeJson r.response of
             Left _ -> state { loading = false, error = networkAddKeyDataError "Something unexpeced broke." }
             Right errors -> state { loading = false, error = errors }

component :: Props -> React.ReactElement
component props =
  React.createElement (T.createClass spec (initialState { keys = props.keys })) props []
