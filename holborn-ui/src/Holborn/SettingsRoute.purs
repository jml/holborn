-- | Module that renders the settings menu on the left and the actual
-- settings content on the right (cf Twitter settings)
module Holborn.SettingsRoute where

import Prelude
import Data.Functor (($>))
import Control.Alt ((<|>))
import Thermite as T
import React.DOM as R
import React.DOM.Props as RP
import React as React
import Control.Monad.Eff.Exception as E
import Network.HTTP.Affjax as AJ
import Web.Cookies as C
import Data.Lens(PrismP, prism, over, lens, view, LensP, set)
import Data.Either (Either(..))
import Holborn.Settings.SSHKeys as SSHKeys
import Data.Foldable (fold)
import Data.Argonaut.Decode (decodeJson)
import Holborn.Fetchable (class Fetchable)
import Text.Parsing.Simple (Parser, string)

import Holborn.Config (makeUrl)
import Debug.Trace

-- We have one route to select where we want to go, and an "OK" route
-- for when the data fetch was successful.
data SettingsRoutes =
    SSHKeySettings
  | SSHKeySettingsOK SSHKeys.State
  | AccountSettings
  | AccountSettingsOK
  | Profile
  | ProfileOK
  | EmailSettings
  | EmailSettingsOK
  | SecuritySettings
  | SecuritySettingsOK
  | RepositorySettings
  | RepositorySettingsOK
  | OrganisationSettings
  | OrganisationSettingsOK


data State = State
  { route :: SettingsRoutes
  , error :: String
  }

routeLens :: LensP State SettingsRoutes
routeLens = lens (\(State s) -> s.route) (\(State s) x -> State (s { route = x }))


settingsRoutes :: Parser String State
settingsRoutes =
      string "ssh-keys" $> (startWithRoute SSHKeySettings)
  <|> string "profile" $> (startWithRoute Profile)
  <|> string "account" $> (startWithRoute AccountSettings)
  <|> string "emails" $> (startWithRoute EmailSettings)
  <|> string "security" $> (startWithRoute SecuritySettings)
  <|> string "repositories" $> (startWithRoute RepositorySettings)
  <|> string "organisations" $> (startWithRoute OrganisationSettings)


instance fetchSettingsRoutes :: Fetchable SettingsRoutes State where
  fetch SSHKeySettings s = do
    r <- AJ.get (makeUrl "/v1/users/alice/keys")
    pure $ case decodeJson r.response of
      Left err -> set routeLens SSHKeySettings s
      Right keys -> set routeLens (SSHKeySettingsOK (SSHKeys.initialState { keys = keys })) s
  fetch a s = pure s


initialState = State
  { route: SSHKeySettings
  , error: "no error"
  }

startWithRoute r = State
  { route: r
  , error: "no error"
  }


data Action =
  SSHKeysAction SSHKeys.Action
  | UpdateRoute SettingsRoutes


_SSHKeysAction :: PrismP Action SSHKeys.Action
_SSHKeysAction = prism SSHKeysAction \action ->
  case action of
    SSHKeysAction x -> Right x
    _ -> Left action


_SSHKeysState :: PrismP SettingsRoutes SSHKeys.State
_SSHKeysState = prism (\state -> SSHKeySettingsOK state) \state->
  case state of
    SSHKeySettingsOK s -> Right s
    _ -> Left state


spec :: forall eff props. T.Spec (err :: E.EXCEPTION, ajax :: AJ.AJAX, cookie :: C.COOKIE | eff) State props Action
spec = container $ fold
       [ T.match _SSHKeysAction (T.focusState routeLens (T.split _SSHKeysState SSHKeys.spec))
--       , T.match _ProfileAction (T.split _ProfileState Profile.spec)
       ]
  where
    container = over T._render \render d p s c ->
      [ R.div [RP.className "container-fluid"]
        [ R.div [RP.className "row"]
          [ R.div [RP.className "col-md-2"] [menu (view routeLens s)]
          , R.div [RP.className "col-md-8"] (render d p s c)
          ]
        ]
      ]

    -- just a link
    lgi label link = R.a [RP.href link, RP.className "list-group-item"] [R.text label]
    -- active
    lgia label link = R.a [RP.href link, RP.className "list-group-item active"] [R.text label]
    -- disabled
    lgid label link = R.a [RP.href link, RP.className "list-group-item disabled"] [R.text label]

    menu :: SettingsRoutes -> React.ReactElement
    menu route =
      R.div [RP.className "list-group"]
      [ let label = "Profile"
            link = "/settings/profile"
        in case route of
           ProfileOK -> lgia label link
           _ -> lgi label link

      , let label = "SSH Keys"
            link = "/settings/ssh-keys"
        in case route of
           SSHKeySettingsOK _ -> lgia label link
           _ -> lgi label link

      , let label = "Account"
            link = "/settings/account"
        in case route of
           AccountSettingsOK -> lgia label link
           _ -> lgid label link

      , let label = "Emails"
            link = "/settings/emails"
        in case route of
           EmailSettingsOK -> lgia label link
           _ -> lgid label link

      , let label = "Security"
            link = "/settings/security"
        in case route of
           SecuritySettingsOK -> lgia label link
           _ -> lgid label link

      , let label = "Repositories"
            link = "/settings/repositories"
        in case route of
           RepositorySettingsOK -> lgia label link
           _ -> lgid label link

      , let label = "Oranisations"
            link = "/settings/organisations"
        in case route of
           OrganisationSettingsOK -> lgia label link
           _ -> lgid label link
      ]
