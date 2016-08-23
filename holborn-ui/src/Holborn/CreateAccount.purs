module Holborn.CreateAccount where

import Prelude
import Thermite as T
import Control.Monad.Eff.Exception as E
import Control.Apply ((<*))
import Control.Alt ((<|>))
import Network.HTTP.Affjax as AJ
import Control.Monad.Aff (attempt)

import React.DOM as R
import React.DOM.Props as RP
import React as React
import Data.Lens (lens, LensP, set, view, toListOf)
import Data.Lens.Traversal (traversed)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Endo (Endo)
import Data.Lens.Types (Fold())
import Data.List (toUnfoldable, List)
import Data.Foldable (intercalate)
import Data.Array (range)
import Control.Monad.Trans (lift)

import Holborn.Fetchable (class Fetchable, fetch, Fetch, decodeResponse)
import Holborn.Config (makeUrl)
import Holborn.ManualEncoding.CreateAccount (CreateAccountData(..), CreateAccountDataError(..), username)
import Holborn.Auth as Auth
import Holborn.Forms as HF

import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Decode (decodeJson)
import Network.HTTP.StatusCode (StatusCode(..))
import Data.Either (Either(..))
import Unsafe.Coerce (unsafeCoerce)

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


spec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX | eff) State props Action
spec = T.simpleSpec performAction render
  where
    render :: forall props. T.Render State props Action
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

    performAction :: forall a. T.PerformAction (err :: E.EXCEPTION, ajax :: AJ.AJAX | eff) State props Action
    performAction CreateAccount props state = do
      T.cotransform (\state -> spy (state { loading = true }))
      r <- lift $ attempt (Auth.post (makeUrl "/v1/create-account") (encodeJson state.formData))
      case r of
         Right {status: StatusCode 201, headers, response } -> do
           traceAnyM (response :: Unit)
           -- TODO redirect to home?
           void $ T.cotransform id

         _ -> void $ T.cotransform id
      void $ T.cotransform (\state -> spy (state { loading = false }))

    performAction (UpdateFormData x) _ state = void $ T.cotransform $ \state -> state { formData = x }
