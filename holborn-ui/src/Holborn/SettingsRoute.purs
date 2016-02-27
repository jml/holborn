-- | Module that renders the settings menu on the left and the actual
-- settings content on the right (cf Twitter settings)
module Holborn.SettingsRoute where

import Prelude (Unit, pure, unit, ($))
import Thermite as T
import React.DOM as R
import React.DOM.Props as RP
import Control.Monad.Eff.Exception as E
import Network.HTTP.Affjax as AJ
import Web.Cookies as C
import Data.Lens(PrismP, LensP, lens, prism, over)
import Data.Either (Either(..))
import Holborn.Routing (SettingsRoutes(..))
import Holborn.Settings.SSHKeys as SSHKeys
import Data.Foldable (fold)

type State = {sshKeysState :: SSHKeys.State}
data Action = SSHKeyAction SSHKeys.Action
type Props = {route :: SettingsRoutes}
initialState = {sshKeysState: SSHKeys.initialState}


_SSHKeyAction :: PrismP Action SSHKeys.Action
_SSHKeyAction = prism SSHKeyAction \ta ->
  case ta of
    SSHKeyAction x -> Right x
    _ -> Left ta


sshkeystate :: LensP State SSHKeys.State
sshkeystate = lens
              _.sshKeysState
              (\state x -> state { sshKeysState = x })

spec :: forall eff. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) State Props Action
spec = container $ fold
       [ T.focus sshkeystate _SSHKeyAction SSHKeys.spec
       ]
  where
    container = over T._render \render d p s c ->
      [ R.div [RP.className "container-fluid"]
        [ R.div [RP.className "row"]
          [ R.div [RP.className "col-md-8"] (render d p s c)
          ]
        ]
      ]

    settings (SSHKeySettingsOK keys) = SSHKeys.component {keys: keys}
    settings AccountSettingsOK = R.div [] [R.text "AccountSettingsOK"]

    settings _ = R.div [] [R.text "loading..."]

    lgi label link = R.a [RP.href link, RP.className "list-group-item"] [R.text label]
    lgia label link = R.a [RP.href link, RP.className "list-group-item active"] [R.text label]

    menu :: SettingsRoutes -> React.ReactElement
    menu route =
      R.div [RP.className "list-group"]
      [ let label = "Profile"
            link = "/#settings/profile"
        in case route of
           ProfileOK -> lgia label link
           _ -> lgi label link

      , let label = "SSH Keys"
            link = "/#settings/ssh-keys"
        in case route of
           SSHKeySettingsOK _ -> lgia label link
           _ -> lgi label link

      , let label = "Account"
            link = "/#settings/account"
        in case route of
           AccountSettingsOK -> lgia label link
           _ -> lgi label link

      , let label = "Emails"
            link = "/#settings/emails"
        in case route of
           EmailSettingsOK -> lgia label link
           _ -> lgi label link

      , let label = "Security"
            link = "/#settings/security"
        in case route of
           SecuritySettingsOK -> lgia label link
           _ -> lgi label link

      , let label = "Repositories"
            link = "/#settings/repositories"
        in case route of
           RepositorySettingsOK -> lgia label link
           _ -> lgi label link

      , let label = "Oranisations"
            link = "/#settings/organisations"
        in case route of
           OrganisationSettingsOK -> lgia label link
           _ -> lgi label link
      ]


component :: Props -> React.ReactElement
component props =
  React.createElement (T.createClass spec initialState) props []
