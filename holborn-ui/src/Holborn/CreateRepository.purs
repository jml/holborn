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
import Data.Lens (set)
import Data.Array (head)

import Holborn.Auth as Auth
import Holborn.Config (makeUrl)
import Holborn.Forms as HF
import Holborn.ManualEncoding.CreateRepository (CreateRepositoryData(..), name, visibility, description, owner, empty, _Public)

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

-- | After fetching server data has succeeded we want to set some default values, e.g. valid owners
fetchUpdate :: Array String -> State -> State
fetchUpdate validRepositoryOwners state@{ formData } =
  state { validRepositoryOwners = validRepositoryOwners, formData = (set owner (head validRepositoryOwners) formData) }

data Action = CreateRepo | UpdateFormData CreateRepositoryData




type Row eff = (err :: E.EXCEPTION, ajax :: AJ.AJAX, navigate :: Navigate | eff) -- TODO move to some common place

readB :: Maybe String -> Boolean
readB (Just "true") = true
readB _ = false

spec :: forall eff props. T.Spec (Row eff) State props Action
spec = T.simpleSpec performAction render
  where
    render :: T.Render State props Action
    render dispatch props ({ formData, formErrors: CreateRepositoryData err, loading, errors, validRepositoryOwners }) _ =
      [ R.h1 [] [R.text "Create a new repository"]
      , R.form [RP.onSubmit onSubmit]
        [ R.text "http://code.space/"
        , ownerCandidateDropdown
        , HF.text "name" err.name name (dispatch <<< UpdateFormData) formData
        , R.button [RP.disabled loading, RP.className "btn btn-default"] [R.text if loading then "Creating ..." else "Create"]
        ]
      , R.ul [] (map (\x -> R.li [] [R.text x]) errors)
      , R.h2 [] [R.text "Visibility"]
      ]
      where
        onSubmit :: React.Event -> T.EventHandler
        onSubmit ev = do
          -- TODO remove unsafeCoerce when
          -- https://github.com/purescript-contrib/purescript-react/pull/84
          -- has been released.
          (unsafeCoerce ev).preventDefault
          dispatch CreateRepo

        -- | Map candidates for ownership to a nice drop-down
        ownerCandidateDropdown :: React.ReactElement
        ownerCandidateDropdown =
          R.select [RP.onChange (\ev -> dispatch (UpdateFormData (set owner (unsafeCoerce ev).target.value formData)))]
          (map (\x -> R.option [] [R.text x]) validRepositoryOwners)

    performAction :: forall eff'. T.PerformAction (Row eff') State props Action
    performAction CreateRepo props state = do
      T.cotransform (_ { loading = true })
      r <- lift $ attempt (Auth.post (makeUrl "/v1/create-repository") (spy (encodeJson state.formData)))
      case Auth.handleResult r of
        Auth.OK (_ :: Unit) -> lift (navigateA "/")
        Auth.FormError errs -> void $ T.cotransform $ _ { formErrors = (spy errs) }
        Auth.OtherError err -> traceAnyM err *> void $ T.cotransform id
      void $ T.cotransform (_ { loading = false })

    performAction (UpdateFormData x) _ state = void $ T.cotransform $ _ { formData = (spy x) }
