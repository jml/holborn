module Holborn.CreateAccount where

import Prelude

import Thermite as T
import Control.Monad.Eff.Exception as E
import Control.Apply ((<*))
import Control.Alt ((<|>))
import Network.HTTP.Affjax as AJ
import Control.Monad.Aff (attempt, Aff)
import Standalone.Router.Dispatch (navigateA)

import React.DOM as R
import React.DOM.Props as RP
import Data.Maybe (Maybe(..))
import Control.Monad.Trans (lift)
import Control.Monad.Eff (Eff)

import Holborn.Config (makeUrl)
import Holborn.ManualEncoding.CreateAccount (CreateAccountData(..), CreateAccountDataError(..), username)
import Holborn.Auth as Auth
import Holborn.Forms as HF

import Data.Argonaut.Encode (encodeJson, class EncodeJson)
import Data.Argonaut.Decode (decodeJson, class DecodeJson)
import Data.Argonaut.Core (Json)
import Network.HTTP.StatusCode (StatusCode(..))
import Data.Either (Either(..))
import Unsafe.Coerce (unsafeCoerce)
import Standalone.Router.Dispatch (Navigate)
import Control.Coroutine (CoTransformer)
import Control.Monad.Eff.Exception (Error)

import Debug.Trace

type State =
    { formErrors :: CreateAccountDataError
    , formData :: CreateAccountData
    , loading :: Boolean
    , errors :: Array String
    }


flashError :: forall a. String -> ({ errors :: Array String | a } -> { errors :: Array String | a })
flashError msg = \s -> s { errors = (s.errors <> [msg]) }

data HandledResult a b c = OK a | FormError b | OtherError c

handleResult :: forall a b. (DecodeJson a, DecodeJson b) => Either Error (AJ.AffjaxResponse Json) -> HandledResult a b String
handleResult r = case r of
  Right {status: StatusCode 201, headers, response } ->
    case decodeJson response of
      Left err -> OtherError (show err)
      Right x -> OK x
  Right {status: StatusCode 400, headers, response } ->
    case decodeJson response of
      Left err -> OtherError (show err)
      Right x -> FormError x
  Left err -> OtherError (show err)
  _ -> OtherError "totally unexpected thing happened"





initialState :: State
initialState =
  { formErrors: CreateAccountDataError {username: Nothing}
  , formData: CreateAccountData {username: ""}
  , loading: false
  , errors: []
  }

data Action = CreateAccount | UpdateFormData CreateAccountData


spec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, navigate :: Navigate | eff) State props Action
spec = T.simpleSpec performAction render
  where
    render :: T.Render State props Action
    render dispatch props ({ formData, formErrors: CreateAccountDataError err, loading, errors }) _ =
      [ R.h1 [] [R.text "Last step! Pick a username for code.space"]
      , R.form [RP.onSubmit onSubmit]
        [ HF.text "Username" err.username username (dispatch <<< UpdateFormData) formData
        , R.button [RP.disabled loading, RP.className "btn btn-default"] [R.text if loading then "Finishing ..." else "Finish setup"]
        ]
      , R.ul [] (map (\x -> R.li [] [R.text x]) errors)
      ]
      where
        onSubmit ev = do
          (unsafeCoerce ev).preventDefault
          dispatch CreateAccount

    performAction :: forall eff'. T.PerformAction (err :: E.EXCEPTION, ajax :: AJ.AJAX, navigate :: Navigate | eff') State props Action
    performAction CreateAccount props state = do
      T.cotransform (\state -> state { loading = true })
      r <- lift $ attempt (Auth.post (makeUrl "/v1/create-account") (encodeJson state.formData))
      case handleResult r of
        OK (_ :: Unit) -> lift (navigateA "/")
        FormError errs -> void $ T.cotransform $ \state -> state { formErrors = errs }
        OtherError _ -> void $ T.cotransform $ id
      void $ T.cotransform (\state -> state { loading = false })

    performAction (UpdateFormData x) _ state = void $ T.cotransform $ \state -> state { formData = x }
