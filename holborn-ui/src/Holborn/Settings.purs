-- | Module that renders the settings menu on the left and the actual
-- settings content on the right (cf Twitter settings)
module Holborn.Settings where

import Prelude
import Thermite as T
import React.DOM as R
import React.DOM.Props as RP
import Control.Monad.Eff.Exception as E
import Network.HTTP.Affjax as AJ
import Web.Cookies as C

import Holborn.Routing (SettingsRoutes(..))
import Holborn.KeySettings as KeySettings

type State = {}
type Action = Unit
type Props = {route :: SettingsRoutes}


spec :: forall eff. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) State Props Action
spec = T.simpleSpec T.defaultPerformAction render
  where
    render _ props _ _ =
      [ R.div [RP.className "container-fluid"]
        [ R.div [RP.className "row"]
          [ R.div [RP.className "col-md-2"] [(menu props.route)]
          , R.div [RP.className "col-md-8"] [(settings props.route)]
          ]
        ]
      ]

    settings (KeySettingsOK keys) = KeySettings.component {keys: keys}
    settings (AccountSettingsOK) = R.div [] [R.text "AccountSettingsOK"]

    lgi label link = R.a [RP.href link, RP.className "list-group-item"] [R.text label]
    lgia label link = R.a [RP.href link, RP.className "list-group-item active"] [R.text label]

    menu :: SettingsRoutes -> React.ReactElement
    menu route =
      R.div [RP.className "list-group"]
      [ let label = "AccountSettingsOK"
            link = "/#settings/account"
        in case route of
           AccountSettingsOK -> lgia label link
           _ -> lgi label link
      , let label = "Manage keys"
            link = "/#settings/keys"
        in case route of
           KeySettingsOK _ -> lgia label link
           _ -> lgi label link
      ]


component :: Props -> React.ReactElement
component props =
  React.createElement (T.createClass spec {}) props []
