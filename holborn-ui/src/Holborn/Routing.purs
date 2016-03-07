module Holborn.Routing where

import Prelude

import Data.Functor ((<$))
import Control.Apply ((<*))
import Control.Alt ((<|>))
import Routing.Match (Match)
import Routing.Match.Class (lit)
import Network.HTTP.Affjax as AJ

import Holborn.SettingsRoute as SettingsRoute
import Holborn.SettingsRoute (SettingsRoutes(..), initialState)
import Holborn.Signin as Signin
import Holborn.Fetchable (class Fetchable, fetch)

import Debug.Trace

instance fetchRootRoutes :: Fetchable RootRoutes where
  fetch (Settings s) = do
    sr <- fetch s.route
    pure (Settings (s { route = sr }))
  fetch x = do
    pure x


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
  Settings (initialState { route = SSHKeySettings }) <$ lit "settings" <* lit "ssh-keys"
  <|> Settings (initialState { route = AccountSettings }) <$ lit "settings" <* lit "account"
  <|> Settings (initialState { route = Profile }) <$ lit "settings" <* lit "profile"
  <|> Settings (initialState { route = EmailSettings }) <$ lit "settings" <* lit "emails"
  <|> Settings (initialState { route = SecuritySettings }) <$ lit "settings" <* lit "security"
  <|> Settings (initialState { route = RepositorySettings }) <$ lit "settings" <* lit "repositories"
  <|> Settings (initialState { route = OrganisationSettings }) <$ lit "settings" <* lit "organisations"
  <|> pure Route404
