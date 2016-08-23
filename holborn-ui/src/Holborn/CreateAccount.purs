module Holborn.CreateAccount where

import Prelude
import Thermite as T
import Control.Monad.Eff.Exception as E
import Control.Apply ((<*))
import Control.Alt ((<|>))
import Network.HTTP.Affjax as AJ

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

import Holborn.Fetchable (class Fetchable, fetch, Fetch, decodeResponse)
import Holborn.Config (makeUrl)
import Holborn.ManualEncoding.CreateAccount (CreateAccountData(..), CreateAccountDataError(..), username)
import Holborn.Auth as Auth
import Holborn.Forms as HF

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
      , R.div []
        [ HF.text "Username" err.username username (dispatch <<< UpdateFormData) formData
        ]
      ]

    performAction :: forall eff' a. T.PerformAction eff' State props Action
    performAction (CreateAccount) props state = void $ T.cotransform id
    performAction (UpdateFormData x) _ state = void $ T.cotransform $ \state -> state { formData = x }
