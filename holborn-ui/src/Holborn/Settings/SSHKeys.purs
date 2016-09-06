-- | ssh key upload

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
import Control.Monad.Trans (lift)
import Network.HTTP.Affjax (AJAX)
import Data.Either (Either(..))
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.List (List(..), (:), toUnfoldable)
import Data.Lens (view, set)
import Data.Maybe (Maybe(..))

import Holborn.ManualEncoding.SSHKeys (Key(..), AddKeyData(..), AddKeyDataError(..), title, key)
import Holborn.Auth as HA
import Holborn.Forms as HF
import Unsafe.Coerce (unsafeCoerce)
import Network.HTTP.StatusCode (StatusCode(..))
import Data.Lens (LensP)
import Data.List (filter)
import Holborn.Config (makeUrl)

import Debug.Trace (traceAnyM, traceAny)


-- The full internal state of this component. Components have state
-- and props. State is internal (e.g. component was loaded) and props
-- are external (e.g. colour of the button).
type State =
  { formErrors :: AddKeyDataError
  , loading :: Boolean
  , keys :: List Key
  , formData :: AddKeyData
  }

-- All possible state-modifying actions for this component.
data Action = AddKey | UpdateFormData AddKeyData | RemoveKey Int

emptyAddKeyData = AddKeyData { key: "", title: "" }
emptyAddKeyDataError = AddKeyDataError { global: Nothing, key: Nothing, title: Nothing }
networkAddKeyDataError msg =  AddKeyDataError { global: Just msg, key: Nothing, title: Nothing }

-- Initial State each time this component is inserted in the DOM.
initialState :: State
initialState =
  { formErrors: emptyAddKeyDataError
  , loading: false
  , keys: Nil
  , formData: emptyAddKeyData
  }


-- We need unsafeCoerce because purescript-react bindings arent
-- exposing target.value yet.
fieldUpdater :: forall a s x. LensP s x -> React.Event -> s -> s
fieldUpdater setter ev state = set setter (unsafeCoerce ev).target.value state


-- TODO: Tom is a bit unsure about the best way to organise the render
-- functions. I think that with more forms some common elements will
-- emerge and we can consolidate then.
labeled label err formEl = case err of
  Nothing -> R.div [ RP.className "form-group" ] [ R.label [] [R.text label],  formEl ]
  Just msg -> R.div [ RP.className "form-group has-error" ] [ R.label [] [R.text (label <> " - " <> msg)],  formEl ]

renderGlobalError err = case err of
  Nothing -> R.text ""
  Just msg -> R.div [] [R.text msg]


spec :: forall eff props. T.Spec (ajax :: AJAX | eff) State props Action
spec = T.simpleSpec performAction render
  where
    -- render is a react-ism. It renders the DOM fragment below and
    -- insert it efficiently by diffing the document-DOM with the
    -- fragment.
    render :: forall props. T.Render State props Action
    render dispatch props ({ formData, formErrors: AddKeyDataError err, loading, keys }) _ =
      [ R.h1 [] [R.text "Manage SSH keys"]
      , R.div [] renderKeyArray
      , R.h2 [] [R.text "Add new key"]
      , renderGlobalError err.global
       , R.form [RP.onSubmit onSubmit]
        [ HF.text "Key name" err.title title (dispatch <<< UpdateFormData) formData
        , HF.textarea "Key" err.key key (dispatch <<< UpdateFormData) formData
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
          [ R.text key.key.comment
          , R.text key.key.fingerprint
          , if key.readonly
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
    performAction :: forall eff a. T.PerformAction (ajax :: AJAX | eff) State props Action
    performAction (UpdateFormData x) props state =  void $ T.cotransform $ \state -> state { formData = x }
    performAction (RemoveKey keyId) props state = do
      T.cotransform (\state -> state { loading = true })
      void $ lift (removeKey keyId)
      void $ T.cotransform (\state -> state { loading = false })

    performAction AddKey _ state  = do
      r <- lift $ HA.post (makeUrl "/v1/user/keys") (encodeJson state.formData)
      void $ case r.status of
         StatusCode 201 -> case decodeJson r.response of
            Left err -> T.cotransform (\state -> state { loading = false, formErrors = networkAddKeyDataError "Something unexpected broke." })
            Right key -> T.cotransform
                (\state -> state { loading = false
                                 , keys = key : state.keys
                                 , formData = emptyAddKeyData
                                 , formErrors = emptyAddKeyDataError
                                 })
         _ -> T.cotransform id


    removeKey :: forall eff. Int -> Aff (ajax :: AJAX | eff) (State -> State)
    removeKey keyId = do
      r <- HA.delete ((makeUrl "/v1/user/keys/") <> show keyId) :: AJ.Affjax eff Unit
      pure $ case r.status of
          StatusCode 204 -> \state -> state { keys = filter (\(Key { id }) -> id /= keyId) state.keys, loading = false }
          _ -> id -- TODO error handling
