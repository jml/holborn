module Holborn.Signin where


import Prelude

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
import Data.Maybe (Maybe(..))
import Web.Cookies as C
import Holborn.Config (makeUrl)

import Holborn.ManualEncoding.Signin (SigninData(..), username, password, SigninDataErrors(..), SigninOK(..))
import Unsafe.Coerce (unsafeCoerce)
import Network.HTTP.StatusCode (StatusCode(..))
import Holborn.Forms as HF

import Debug.Trace (traceAnyM)


type State = { loading :: Boolean
             , formData :: SigninData
             , formErrors :: SigninDataErrors
             }
data Action = SignIn | UpdateFormData SigninData


initialState =
  { loading: false
  , formData: SigninData {username: "", password: ""}
  , formErrors: SigninDataErrors { username: Nothing, password: Nothing }
  }


spec :: forall eff props. T.Spec (ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) State props Action
spec = T.simpleSpec performAction render
  where
    render :: T.Render State props Action
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
      runAff (\err -> traceAnyM err >>= \_ -> k handleSigninError) k (runSignin state)
    performAction (UpdateFormData x) props state k = k $ \state -> state { formData = x }

    -- TODO show an error if we get a network error.
    handleSigninError :: State -> State
    handleSigninError state = state { loading = false }

    runSignin :: forall eff'. State -> Aff (ajax :: AJAX, cookie :: C.COOKIE | eff') (State -> State)
    runSignin state = do
      r <- AJ.post (makeUrl "/v1/signin") (encodeJson state.formData)
      case r.status of
        StatusCode 200 -> case decodeJson r.response of
          Left err -> return (\state -> state { loading = false })
          Right (SigninOK { token }) -> do
            liftEff $ C.setCookie "auth-token" token {path: "/"}
            return (\state -> state { loading = false })
        StatusCode 400 -> case decodeJson r.response of
          Left _ -> return (\state -> state { loading = false }) -- TODO error handling
          Right errors -> return (\state -> state { loading = false, formErrors = errors })
        -- TODO: Complete the pattern match: server can return anything, and
        -- client-side code should be ready to handle it.
