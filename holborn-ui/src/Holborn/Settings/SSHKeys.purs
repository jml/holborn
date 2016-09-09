-- | ssh key upload

module Holborn.Settings.SSHKeys where

import Prelude

import React as React
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T
import Network.HTTP.Affjax as AJ
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Trans (lift)
import Network.HTTP.Affjax (AJAX)
import Data.Argonaut.Encode (encodeJson)
import Data.List (List(..), (:), toUnfoldable, filter)
import Data.Maybe (Maybe(..))

import Holborn.ManualEncoding.SSHKeys (Key(..), AddKeyData(..), title, key)
import Holborn.Auth as HA
import Holborn.Forms as HF
import Unsafe.Coerce (unsafeCoerce)
import Network.HTTP.StatusCode (StatusCode(..))
import Data.Lens (set, LensP)
import Holborn.Config (makeUrl)



-- The full internal state of this component. Components have state
-- and props. State is internal (e.g. component was loaded) and props
-- are external (e.g. colour of the button).
type State =
  { formErrors :: AddKeyData
  , loading :: Boolean
  , keys :: List Key
  , formData :: AddKeyData
  }

-- All possible state-modifying actions for this component.
data Action = AddKey | UpdateFormData AddKeyData | RemoveKey Int

emptyAddKeyData = AddKeyData { key: Nothing, title: Nothing }
networkAddKeyDataError _ =  AddKeyData { key: Nothing, title: Nothing } -- TODO global errors need to go somewhere

-- Initial State each time this component is inserted in the DOM.
initialState :: State
initialState =
  { formErrors: emptyAddKeyData
  , loading: false
  , keys: Nil
  , formData: emptyAddKeyData
  }


-- We need unsafeCoerce because purescript-react bindings arent
-- exposing target.value yet.
fieldUpdater :: forall s x. LensP s x -> React.Event -> s -> s
fieldUpdater setter ev state = set setter (unsafeCoerce ev).target.value state


-- TODO: Tom is a bit unsure about the best way to organise the render
-- functions. I think that with more forms some common elements will
-- emerge and we can consolidate then.
labeled label err formEl = case err of
  Nothing -> R.div [ RP.className "form-group" ] [ R.label [] [R.text label],  formEl ]
  Just msg -> R.div [ RP.className "form-group has-error" ] [ R.label [] [R.text (label <> " - " <> msg)],  formEl ]


spec :: forall eff props. T.Spec (ajax :: AJAX | eff) State props Action
spec = T.simpleSpec performAction render
  where
    -- render is a react-ism. It renders the DOM fragment below and
    -- insert it efficiently by diffing the document-DOM with the
    -- fragment.
    render :: forall props. T.Render State props Action
    render dispatch props ({ formData, formErrors: AddKeyData err, loading, keys }) _ =
      [ R.h1 [] [R.text "Manage SSH keys"]
      , R.div [] renderKeyArray
      , R.h2 [] [R.text "Add new key"]
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
          , R.span [RP.className "key"] [R.text key.key.type_]
          , if key.readonly
            then R.span [RP.className "readonly"] [R.text "readonly | "]
            else R.span [RP.className "write"] [R.text "write | "]
          , if key.verified
            then R.span [RP.className "verified"] [R.text "verified"]
            else R.span [RP.className "notverified"] [R.text "not verified"]
          , R.a [RP.onClick \ev -> do
                    (unsafeCoerce ev).preventDefault
                    dispatch (RemoveKey key.id)
                ] [R.text "| Remove this key"]
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
      r <- lift $ attempt $ HA.post (makeUrl "/v1/user/keys") (encodeJson state.formData)
      case HA.handleResult r of
        HA.OK key -> void $ T.cotransform $ \state -> state { keys = key : state.keys
                                                            , formErrors = emptyAddKeyData
                                                            , formData = emptyAddKeyData
                                                            }
        HA.FormError errs -> void $ T.cotransform $ \state -> state { formErrors = errs }
        HA.OtherError _ -> void $ T.cotransform (\state -> state { formErrors = networkAddKeyDataError "Something unexpected broke." })
      void $ T.cotransform (\state -> state { loading = false })

    removeKey :: forall eff. Int -> Aff (ajax :: AJAX | eff) (State -> State)
    removeKey keyId = do
      r <- HA.delete ((makeUrl "/v1/user/keys/") <> show keyId) :: AJ.Affjax eff Unit
      pure $ case r.status of
          StatusCode 204 -> \state -> state { keys = filter (\(Key { id }) -> id /= keyId) state.keys, loading = false }
          _ -> id -- TODO error handling
