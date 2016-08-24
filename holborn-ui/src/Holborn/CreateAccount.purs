module Holborn.CreateAccount where

import Prelude (Unit, void, bind, id, ($), (<<<))

import Thermite as T
import Control.Monad.Eff.Exception as E
import Control.Apply ((<*))
import Control.Alt ((<|>))
import Network.HTTP.Affjax as AJ
import Control.Monad.Aff (attempt)
import Standalone.Router.Dispatch (navigate)

import React.DOM as R
import React.DOM.Props as RP
import Data.Maybe (Maybe(..))
import Control.Monad.Trans (lift)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)

import Holborn.Config (makeUrl)
import Holborn.ManualEncoding.CreateAccount (CreateAccountData(..), CreateAccountDataError(..), username)
import Holborn.Auth as Auth
import Holborn.Forms as HF

import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Decode (decodeJson)
import Network.HTTP.StatusCode (StatusCode(..))
import Data.Either (Either(..))
import Unsafe.Coerce (unsafeCoerce)
import Standalone.Router.Dispatch (Navigate)

import Debug.Trace

type State =
    { formErrors :: CreateAccountDataError
    , formData :: CreateAccountData
    , loading :: Boolean
    }

initialState :: State
initialState =
  { formErrors: CreateAccountDataError {username: Nothing}
  , formData: CreateAccountData {username: ""}
  , loading: false
  }

data Action = CreateAccount | UpdateFormData CreateAccountData


spec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, navigate :: Navigate | eff) State props Action
spec = T.simpleSpec performAction render
  where
    render :: T.Render State props Action
    render dispatch props ({ formData, formErrors: CreateAccountDataError err, loading }) _ =
      [ R.h1 [] [R.text "Last step! Pick a username for code.space"]
      , R.form [RP.onSubmit onSubmit]
        [ HF.text "Username" err.username username (dispatch <<< UpdateFormData) formData
        , R.button [RP.disabled loading, RP.className "btn btn-default"] [R.text if loading then "Finishing ..." else "Finish setup"]
        ]
      ]
      where
        onSubmit ev = do
          (unsafeCoerce ev).preventDefault
          dispatch CreateAccount

    performAction :: forall eff'. T.PerformAction (err :: E.EXCEPTION, ajax :: AJ.AJAX, navigate :: Navigate | eff') State props Action
    performAction CreateAccount props state = do
      T.cotransform (\state -> spy (state { loading = true }))
      r <- lift $ attempt (Auth.post (makeUrl "/v1/create-account") (encodeJson state.formData))
      case r of
         Right {status: StatusCode 201, headers, response } -> do
           traceAnyM (response :: Unit)
           -- TODO redirect to home?
           void $ T.cotransform id

         _ -> void $ T.cotransform id

      -- The type checker isn't clever enough to figure out the effect
      -- of navigate so we need to specify it explicityly.
      lift $ liftEff $ (navigate "/settings/ssh-keys" :: Eff (err :: E.EXCEPTION, ajax :: AJ.AJAX, navigate :: Navigate | eff') Unit)
      void $ T.cotransform (\state -> spy (state { loading = false }))

    performAction (UpdateFormData x) _ state = void $ T.cotransform $ \state -> state { formData = x }
