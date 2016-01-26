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
import Control.Monad.Eff.Console (log, CONSOLE())

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


data Action = AddKey | LoadData

initialState :: State
initialState =
  { valid: false
  , keys: ["key1"]
  }


spec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) State props Action
spec = T.simpleSpec performAction render
  where
    render :: T.Render State props Action
    render dispatch _ s _ =
      [ R.h1 [] [R.text "Key settings"]
      , R.form []
        [ R.textarea [RP.value ("hello " ++ (show s.valid))] []
        , R.button [] [R.text "add new key" ]
        ]
      ]

    performAction AddKey props state k = k $ state { valid = true }
    performAction LoadData props state k = k $ state { valid = true }


componentDidMount :: forall props state eff. (React.ReactThis props state -> Action -> T.EventHandler) -> R.ComponentDidMount props state (console :: CONSOLE | eff)
componentDidMount dispatch this = do
  dispatch this LoadData


component :: forall eff props. props -> R.ReactElement
component props =
  let rspec = T.createReactSpec spec initialState
      cls = R.createClass ((_.spec rspec) { componentDidMount = (componentDidMount (_.dispatcher rspec))})
  in R.createElement cls props []
