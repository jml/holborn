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
import React as React
import React.DOM as R
import React.DOM.Props as RP
import Standalone.Router.Dispatch (Navigate, navigateA)
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)

import Holborn.Auth as Auth
import Holborn.Config (makeUrl)
import Holborn.Forms as HF
import Holborn.ManualEncoding.CreateRepository (CreateRepositoryData(..), repoName, visibility, description, empty)

import Debug.Trace


type State =
    { formErrors :: CreateRepositoryData
    , formData :: CreateRepositoryData
    , loading :: Boolean
    , errors :: Array String
    , validRepositoryOwners :: Array String
    }

initialState :: State
initialState =
  { formErrors: empty
  , formData: empty
  , loading: false
  , errors: []
  , validRepositoryOwners: []
  }

data Action = CreateRepo | UpdateFormData CreateRepositoryData

type Row eff = (err :: E.EXCEPTION, ajax :: AJ.AJAX, navigate :: Navigate | eff) -- TODO move to some common place

spec :: forall eff props. T.Spec (Row eff) State props Action
spec = T.simpleSpec performAction render
  where
    render :: T.Render State props Action
    render dispatch props ({ formData, formErrors: CreateRepositoryData err, loading, errors, validRepositoryOwners }) _ =
      [ R.h1 [] [R.text "Create a new repository"]
      , R.form [RP.onSubmit onSubmit]
        [ R.text "http://code.space/"
        , ownerCandidateDropdown
        , HF.text "name" err.repoName repoName (dispatch <<< UpdateFormData) formData
        , R.button [RP.disabled loading, RP.className "btn btn-default"] [R.text if loading then "Creating ..." else "Create"]
        ]
      , R.ul [] (map (\x -> R.li [] [R.text x]) errors)
      ]
      where
        onSubmit :: React.Event -> T.EventHandler
        onSubmit ev = do
          (unsafeCoerce ev).preventDefault
          dispatch CreateRepo

        -- | Map candidates for ownership to a nice drop-down
        ownerCandidateDropdown :: React.ReactElement
        ownerCandidateDropdown = R.select [] (map (\x -> R.option [] [R.text x]) validRepositoryOwners)

    performAction :: forall eff'. T.PerformAction (Row eff') State props Action
    performAction CreateRepo props state = do
      T.cotransform (\state -> state { loading = true })
      r <- lift $ attempt (Auth.post (makeUrl "/v1/create-repository") (encodeJson state.formData))
      case Auth.handleResult r of
        Auth.OK (_ :: Unit) -> lift (navigateA "/")
        Auth.FormError errs -> void $ T.cotransform $ \state -> state { formErrors = (spy errs) }
        Auth.OtherError err -> traceAnyM err *> void $ T.cotransform id
      void $ T.cotransform (\state -> state { loading = false })

    performAction (UpdateFormData x) _ state = void $ T.cotransform $ \state -> state { formData = x }
