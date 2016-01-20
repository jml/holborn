module Components.Validated where

import Prelude
import qualified Thermite as T
import qualified React.DOM as R
import qualified React as R
import qualified React.DOM.Props as RP
import qualified Thermite.Aff as TA
import qualified Network.HTTP.Affjax as AJ
import Control.Monad.Aff
import qualified Control.Monad.Eff.Exception as E
import Network.HTTP.StatusCode (StatusCode(..))
import Data.Argonaut.Encode (gEncodeJson, EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)

import Data.Argonaut.Core (Json(), fromObject, toObject, toString)
import Data.StrMap as SM
import Data.String (indexOf)

import Debug.Trace (traceAnyM)
import Unsafe.Coerce (unsafeCoerce)
import Data.Generic (Generic)
import Data.Maybe
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff.Console (print)
import qualified Data.Either as Either
import qualified Web.Cookies as C
import Components.Response (Response(..))

import Data.Foreign (Foreign())
import Holborn.Routing (RootRoutes(..))

type SignupData =
  { username :: String
  , email :: String
  , password :: String
  }

data SignupOK = JWT String
data SignupError = EUserExists String | EOtherError

instance decodeSignupOK :: DecodeJson SignupOK where
  decodeJson json =
    case dec of
      Nothing -> Either.Left "could not decode OK"
      Just x -> Either.Right x
    where
      dec :: Maybe SignupOK
      dec = do
        r <- toString json
        pure (JWT r)

instance decodeSignupError :: DecodeJson SignupError where
  decodeJson json = pure (EUserExists "user")


-- TODO - The API data structures & JSON encodings should be derived
-- from servant.
signupDataJSON :: SignupData -> Json
signupDataJSON a = fromObject
    $ SM.insert "username" (encodeJson a.username)
    $ SM.insert "email" (encodeJson a.email)
    $ SM.singleton "password" (encodeJson a.password)


type InputState =
  { emailValid :: Boolean
  , usernameValid :: Boolean
  , passwordValid :: Boolean
  , signupData :: SignupData
  , currentRoute :: RootRoutes
  }

canSubmit :: InputState -> Boolean
canSubmit s = not (s.emailValid && s.usernameValid && s.passwordValid)

data InputAction =
  ValidateUsername String
  | ValidateEmail String
  | ValidatePassword String
  | Submit
  | UpdateRoute RootRoutes

initialState :: InputState
initialState =
  { usernameValid: false
  , passwordValid: false
  , emailValid: false
  , signupData: { username: "", email: "", password: "" }
  , currentRoute: RouteB
  }


simpleButtonSpec :: forall eff props. T.Spec eff Unit props Unit
simpleButtonSpec = T.simpleSpec T.defaultPerformAction render
  where
    render _ _ _ _ = [ R.button [] [ R.text "simpleButton"] ]
simpleButton props = R.createElement (T.createClass simpleButtonSpec unit) props []


validatedInput :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) InputState props InputAction
validatedInput = T.simpleSpec performAction render
  where
    render :: T.Render InputState props InputAction
    render dispatch _ s _ =
      [ R.form [ RP.onSubmit \e -> do
                    (unsafeCoerce e).preventDefault
                    dispatch Submit
               ]
        [ R.div [ RP.className (if s.usernameValid then "form-group has-success" else "form-group has-error")
                ] [ R.input [ RP._type "text"
                            , RP.placeholder "username"
                            , RP.className "form-control"
                            , RP.onBlur \e -> dispatch (ValidateUsername (unsafeCoerce e).target.value)
                            ] []
                  ]
        , case s.currentRoute of
             RouteB -> simpleButton [RP._type "text"]
             RouteA _ -> R.text "routea"
             _ -> R.text "404"

        , R.div [ RP.className (if s.emailValid then "form-group has-success" else "form-group has-error")
                ] [ R.input [ RP._type "text"
                            , RP.placeholder "email"
                            , RP.className "form-control"
                            , RP.onBlur \e -> dispatch (ValidateEmail (unsafeCoerce e).target.value)
                            ] []
                  ]
        , R.div [ RP.className (if s.passwordValid then "form-group has-success" else "form-group has-error")
                ] [ R.input [ RP._type "password"
                            , RP.placeholder "password"
                            , RP.className "form-control"
                            , RP.onBlur \e -> dispatch (ValidatePassword (unsafeCoerce e).target.value)
                            ] []
                  ]
        , R.div [] [ R.button [ RP._type "submit"
                              , RP.disabled (canSubmit s)
                              ] [ R.text "Sign up" ]
                   ]
        ] -- end form
      ]

    doGet :: forall eff2 props. InputAction -> props -> InputState -> Aff (ajax :: AJ.AJAX, cookie :: C.COOKIE | eff2) InputState
    doGet Submit _ state = do
      r <- AJ.post "http://127.0.0.1:8002/users/signup" (signupDataJSON state.signupData)
      let response = decodeJson r.response :: Either.Either String (Response SignupOK SignupError)
      traceAnyM response
      case r.status of
         StatusCode 201 -> case response of
                             Either.Right (OK jwt) -> do
                               liftEff $ C.setCookie "jwt" jwt {}
                               pure state
                             Either.Right (Error (EUserExists _)) -> pure $ state { usernameValid = false }
                             Either.Right (Error EOtherError) -> pure $ state { usernameValid = false }
         StatusCode 204 -> pure state
         _ -> pure state

    doGet (ValidateUsername username) _ state = do
         -- TODO check whether username is taken
         pure $ state { usernameValid = true, signupData = state.signupData { username = username } }

    doGet (ValidateEmail email) _ state = do
         let valid = case indexOf "@" email of
                       Just _ -> true
                       _ -> false
         pure $ state { emailValid = valid, signupData = state.signupData { email = email } }

    doGet (ValidatePassword password) _ state = do
         pure $ state { passwordValid = true, signupData = state.signupData { password = password } }

    performAction :: forall eff. T.PerformAction (err :: E.EXCEPTION, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) InputState props InputAction
    performAction action@(UpdateRoute r) props state k = k $ state { currentRoute = r }
    performAction action@(ValidateUsername u) props state k = do
        k $ state { usernameValid = false } -- Set UI validity to false immediately.
        TA.asyncOne' doGet action props state k
    performAction action props state k = TA.asyncOne' doGet action props state k
