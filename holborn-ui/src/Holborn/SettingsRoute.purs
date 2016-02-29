-- | Module that renders the settings menu on the left and the actual
-- settings content on the right (cf Twitter settings)
module Holborn.SettingsRoute where

import Prelude
import Thermite as T
import React.DOM as R
import React.DOM.Props as RP
import Control.Monad.Eff.Exception as E
import Network.HTTP.Affjax as AJ
import Web.Cookies as C
import Data.Lens(PrismP, LensP, lens, prism, over, set)
import Data.Either (Either(..))
import Holborn.Routing (SettingsRoutes(..))
import Holborn.Settings.SSHKeys as SSHKeys
import Data.Foldable (fold)
import Data.Maybe (maybe)

import Debug.Trace

type State =
  { sshKeysState :: SSHKeys.State
  , route :: SettingsRoutes
  , error :: String
  }
data Action = SSHKeysAction SSHKeys.Action
type Props = {route :: SettingsRoutes}
initialState = {sshKeysState: SSHKeys.initialState, route: SSHKeySettings, error: "no error"}


_SSHKeysAction :: PrismP Action SSHKeys.Action
_SSHKeysAction = prism SSHKeysAction \action ->
  case action of
    SSHKeysAction x -> Right x
    _ -> Left action


_SSHKeysState :: PrismP State SSHKeys.State
_SSHKeysState = prism (\state -> initialState { sshKeysState = state, route = SSHKeySettingsOK state.keys, error = "" }) \state ->
  case state.route of
    SSHKeySettingsOK keys -> Right (state.sshKeysState { keys = keys })
    _ -> Left state


spec :: forall eff. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) State Props Action
spec = container $ fold
       [ T.match _SSHKeysAction (T.split _SSHKeysState SSHKeys.spec)
       , T.match _AccountAction (T.split _AccountState Account.spec)
       ]
  where
    container = over T._render \render d p s c ->
      [ R.div [RP.className "container-fluid"]
        [ R.div [RP.className "row"]
          [ R.div [RP.className "col-md-2"] [R.text s.error, menu p.route]
          , R.div [RP.className "col-md-8"] (render d p s c)
          ]
        ]
      ]

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
  React.createElement (T.createClass spec (initialState { route = props.route })) props []
