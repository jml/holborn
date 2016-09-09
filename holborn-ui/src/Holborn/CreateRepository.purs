module Holborn.CreateRepository where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((<*))
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception as E
import Control.Monad.Trans (lift)
import Data.Argonaut.Encode (encodeJson)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax as AJ
import React.DOM as R
import React.DOM.Props as RP
import Standalone.Router.Dispatch (Navigate, navigateA)
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)

import Holborn.Auth as Auth
import Holborn.Config (makeUrl)
import Holborn.Forms as HF
import Holborn.ManualEncoding.CreateAccount (CreateAccountData(..), username)

import Debug.Trace


type State =
    { formErrors :: CreateAccountData
    , formData :: CreateAccountData
    , loading :: Boolean
    , errors :: Array String
    }

initialState :: State
initialState =
  { formErrors: CreateAccountData {username: Nothing}
  , formData: CreateAccountData {username: Nothing}
  , loading: false
  , errors: []
  }

data Action = CreateRepo | UpdateFormData CreateAccountData


spec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, navigate :: Navigate | eff) State props Action
spec = T.simpleSpec performAction render
  where
    render :: T.Render State props Action
    render dispatch props ({ formData, formErrors: CreateAccountData err, loading, errors }) _ =
      [ R.h1 [] [R.text "Create a new repository"]
      , R.form [RP.onSubmit onSubmit]
        [ HF.text "http://code.space/me/" err.username username (dispatch <<< UpdateFormData) formData
        , R.button [RP.disabled loading, RP.className "btn btn-default"] [R.text if loading then "Finishing ..." else "Finish setup"]
        ]
      , R.ul [] (map (\x -> R.li [] [R.text x]) errors)
      ]
      where
        onSubmit ev = do
          (unsafeCoerce ev).preventDefault
          dispatch CreateRepo

    performAction :: forall eff'. T.PerformAction (err :: E.EXCEPTION, ajax :: AJ.AJAX, navigate :: Navigate | eff') State props Action
    performAction CreateRepo props state = do
      T.cotransform (\state -> state { loading = true })
      r <- lift $ attempt (Auth.post (makeUrl "/v1/create-repository") (encodeJson state.formData))
      case Auth.handleResult r of
        Auth.OK (_ :: Unit) -> lift (navigateA "/")
        Auth.FormError errs -> void $ T.cotransform $ \state -> state { formErrors = errs }
        Auth.OtherError _ -> void $ T.cotransform $ id
      void $ T.cotransform (\state -> state { loading = false })

    performAction (UpdateFormData x) _ state = void $ T.cotransform $ \state -> state { formData = x }
