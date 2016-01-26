module Holborn.KeySettings where

import Prelude
import qualified Thermite as T
import qualified React.DOM as R
import qualified React as R
import qualified React.DOM.Props as RP
import qualified Thermite.Aff as TA
import Control.Monad.Aff
import qualified Control.Monad.Eff.Exception as E
import Network.HTTP.StatusCode (StatusCode(..))
import Control.Monad.Eff.Console (CONSOLE())

import Components.Validated as V

import Data.Foreign (Foreign())
import Holborn.Routing (RootRoutes(..), rootRoutes)
import qualified Web.Cookies as C
import Data.Maybe
import Routing (matches)
import qualified Network.HTTP.Affjax as AJ


type State =
  { valid :: Boolean
  , keys :: Array String
  }


data Action = AddKey

initialState :: State
initialState =
  { valid: false
  , keys: ["key1"]
  }


spec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) State props Action
spec = T.simpleSpec performAction render
  where
    render :: T.Render State props Action
    render dispatch _ s _ = [ R.text "keysettings" ]

    performAction AddKey props state k = k $ state { valid = true }


component :: forall eff props. props -> R.ReactElement
component props = R.createElement (T.createClass spec initialState) props []
