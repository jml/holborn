module Holborn.Signin where


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
import Web.Cookies as C

import Holborn.Auth as HA
import Holborn.ManualEncoding.Signin (SigninData(..), username, password, SigninDataErrors(..), SigninOK(..))
import Unsafe.Coerce (unsafeCoerce)
import Network.HTTP.StatusCode (StatusCode(..))
import Data.Lens (LensP, view, set)
import Holborn.Forms as HF

import Debug.Trace (traceAnyM)


type State = { loading :: Boolean, formData :: SigninData, formErrors :: SigninDataErrors }
data Action = SignIn | UpdateFormData SigninData
type Props = {}


initialState =
  { loading: false
  , formData: SigninData {username: "", password: ""}
  , formErrors: SigninDataErrors { username: Nothing, password: Nothing }
  }


spec :: forall eff. T.Spec (ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) State Props Action
spec = T.simpleSpec performAction render
  where
    render :: T.Render State Props Action
    render dispatch _ ({loading, formData, formErrors: SigninDataErrors errs}) _ =
      [ R.h1 [] [R.text "Sign in"]
      , R.form [RP.onSubmit onSubmit]
        [ (HF.text "Username" errs.username username (dispatch <<< UpdateFormData) formData)
        , (HF.password "Password" errs.password password (dispatch <<< UpdateFormData) formData)
        , R.button [RP._type "submit", RP.disabled loading, RP.className "btn btn-default"]
          [R.text if loading then "Signing in ..." else "Sign in"]
        ]
      ]
      where
        onSubmit ev = do
          (unsafeCoerce ev).preventDefault
          dispatch SignIn

    performAction SignIn props state k = do
      k $ \state -> state { loading = true }
      runAff (\err -> traceAnyM err >>= \_ -> k id) k (runSignin state)
    performAction (UpdateFormData x) props state k = k $ \state -> state { formData = x }

    --runSignin :: forall eff. State -> Aff (ajax :: AJAX, cookie :: C.COOKIE | eff) State
    runSignin state = do
      r <- AJ.post "http://127.0.0.1:8002/v1/signin" (encodeJson state.formData)
      case r.status of
        StatusCode 201 -> case decodeJson r.response of
          Left err -> return (\state -> state { loading = false })
          Right (SigninOK { token }) -> do
            liftEff $ C.setCookie "auth-token" token {}
            return (\state -> state { loading = false })
        StatusCode 400 -> case decodeJson r.response of
          Left _ -> return (\state -> state { loading = false }) -- TODO error hanldinf
          Right errors -> return (\state -> state { loading = false, formErrors = errors })



component :: Props -> React.ReactElement
component props =
  React.createElement (T.createClass spec initialState) props []
