-- | Example module for how to do an UI element in our UI.

module Holborn.Settings.SSHKeys where


import Prelude

import React as React
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T
import Network.HTTP.Affjax as AJ
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (runAff, Aff)
import Network.HTTP.Affjax (AJAX)
import Data.Either (Either(..))
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.List (List(..), (:), toUnfoldable)
import Data.Lens (view, set)
import Data.Maybe (Maybe(..))
import Web.Cookies as C

import Holborn.ManualEncoding.Keys (Key(..), AddKeyData(..), AddKeyDataError(..), title, key)
import Holborn.Auth as HA
import Holborn.Forms as HF
import Unsafe.Coerce (unsafeCoerce)
import Network.HTTP.StatusCode (StatusCode(..))
import Data.Lens (LensP)
import Data.List (filter)

import Debug.Trace (traceAnyM, traceAny)


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
data Action = AddKey | UpdateKeyData AddKeyData | RemoveKey Int

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

-- type Props = {keys :: List Key}


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


spec :: forall eff props. T.Spec (ajax :: AJAX, cookie :: C.COOKIE | eff) State props Action
spec = T.simpleSpec performAction render
  where
    -- render is a react-ism. It renders the DOM fragment below and
    -- insert it efficiently by diffing the document-DOM with the
    -- fragment.
    render :: forall props. T.Render State props Action
    render dispatch props ({ addKeyData, error: AddKeyDataError err, loading, keys }) _ =
      [ R.h1 [] [R.text "Manage SSH keys"]
      , R.div [] renderKeyArray
      , R.h2 [] [R.text "Add new key"]
      , renderGlobalError err.global
       , R.form [RP.onSubmit onSubmit]
        [ HF.text "Key name" err.title title (dispatch <<< UpdateKeyData) addKeyData
        , HF.textarea "Key" err.key key (dispatch <<< UpdateKeyData) addKeyData
        , R.button [RP.disabled loading, RP.className "btn btn-default"] [R.text if loading then "Adding key ..." else "Add new key"]
        ]
      ]
      where
        onSubmit ev = do
          (unsafeCoerce ev).preventDefault
          dispatch AddKey
        renderKeyArray = toUnfoldable (map renderOneKey keys)
        renderOneKey (Key key) =
          R.div []
          [ R.text key.title
          , R.text key.key.fingerprint
          , if key.read_only
            then R.span [RP.className "readonly"] [R.text "readonly"]
            else R.span [RP.className "write"] [R.text "write"]
          , if key.verified
            then R.span [RP.className "verified"] [R.text "verified"]
            else R.span [RP.className "notverified"] [R.text "not verified"]
          , R.a [RP.onClick \ev -> do
                    (unsafeCoerce ev).preventDefault
                    dispatch (RemoveKey key.id)
                ] [R.text "Remove this key"]
          ]

    -- performAction is a purescript-thermite callback. It takes an
    -- action and modifies the state by calling the callback k and
    -- passing it the modified state.
    performAction :: forall eff a. T.PerformAction (ajax :: AJAX, cookie :: C.COOKIE | eff) State props Action
    performAction (UpdateKeyData x) props state k = k $ \state -> state { addKeyData = x }
    performAction (RemoveKey keyId) props state k = do
      k (\state -> state { loading = true })
      runAff (\_ -> k id) k (removeKey keyId)

    performAction AddKey props state k = do
      k (\state -> state { loading = true })
      runAff (\err -> traceAnyM err >>= \_ -> k $ \staet -> state { error = networkAddKeyDataError "No network connection. Please try again later"}) k (addKey state)

    -- Server fetching can go in many ways and we'll need to reflect
    -- errors in the state and allow users to move on from there
    -- (e.g. re-enable buttons for retry, display actual invalid input
    -- errors etc).
    addKey :: forall eff. State -> Aff (ajax :: AJAX, cookie :: C.COOKIE | eff) (State -> State)
    addKey state = do
      r <- HA.post "http://127.0.0.1:8002/v1/user/keys" (encodeJson state.addKeyData)
      return case r.status of
         StatusCode 201 -> case decodeJson r.response of
             Left err -> \state -> state { loading = false, error = networkAddKeyDataError "Something unexpeced broke." }
             Right key -> \state -> state { loading = false, keys = key : state.keys, addKeyData = emptyAddKeyData, error = emptyAddKeyDataError }

         -- Note that by calling `decodeJson` in the 201 branch the
         -- type inference decided that the response must be JSON so
         -- we need to send back valid JSON in the 400 case as well.
         StatusCode 400 -> case decodeJson r.response of
             Left _ -> \state -> state { loading = false, error = networkAddKeyDataError "Something unexpeced broke." }
             Right errors -> \state -> state { loading = false, error = errors }

    removeKey :: forall eff. Int -> Aff (ajax :: AJAX, cookie :: C.COOKIE | eff) (State -> State)
    removeKey keyId = do
      r <- HA.delete ("http://127.0.0.1:8002/v1/user/keys/" ++ show keyId) :: AJ.Affjax (cookie :: C.COOKIE | eff) Unit
      return $ case r.status of
          StatusCode 204 -> \state -> state { keys = filter (\(Key { id }) -> id /= keyId) state.keys }
          _ -> id -- TODO error handling

component :: forall props. props -> React.ReactElement
component props =
  React.createElement (T.createClass spec initialState) props []
