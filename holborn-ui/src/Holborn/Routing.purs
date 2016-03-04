module Holborn.Routing where

import Prelude

import Data.Functor ((<$))
import Control.Apply ((<*))
import Control.Alt ((<|>))
import Routing.Match (Match)
import Routing.Match.Class (lit)
import Network.HTTP.Affjax as AJ

import Holborn.SettingsRoute as SettingsRoute
import Holborn.SettingsRoute (SettingsRoutes(..), startWithRoute)
import Holborn.Signin as Signin

import Debug.Trace


data RootRoutes =
    EmptyRoute
  | Settings SettingsRoute.State
  | Route404
  | ErrorRoute
  | SigninRoute Signin.State


-- TODO tom: Routes should really be "invertible" so I can create a
-- KeySettings route string from the value.
rootRoutes :: Match RootRoutes
rootRoutes =
  Settings (startWithRoute SSHKeySettings) <$ lit "settings" <* lit "ssh-keys"
  <|> Settings (startWithRoute AccountSettings) <$ lit "settings" <* lit "account"
  <|> Settings (startWithRoute Profile) <$ lit "settings" <* lit "profile"
  <|> Settings (startWithRoute EmailSettings) <$ lit "settings" <* lit "emails"
  <|> Settings (startWithRoute SecuritySettings) <$ lit "settings" <* lit "security"
  <|> Settings (startWithRoute RepositorySettings) <$ lit "settings" <* lit "repositories"
  <|> Settings (startWithRoute OrganisationSettings) <$ lit "settings" <* lit "organisations"
  <|> pure Route404
