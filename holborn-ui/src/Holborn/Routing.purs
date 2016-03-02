module Holborn.Routing where

import Prelude

import Data.Functor ((<$))
import Control.Apply ((<*))
import Control.Alt ((<|>))
import Routing.Match (Match)
import Routing.Match.Class (lit)

import Holborn.SettingsRoute as SettingsRoute
import Holborn.SettingsRoute (SettingsRoutes(..), initialState)
import Holborn.Signin as Signin
import Holborn.Fetchable (class Fetchable, fetch)


instance fetchRootRoutes :: Fetchable RootRoutes where
  fetch (Settings s route) = do
    sr <- fetch route
    pure (Settings s sr)
  fetch x = pure x


data RootRoutes =
    EmptyRoute
  | Settings SettingsRoute.State SettingsRoutes
  | Route404
  | ErrorRoute
  | SigninRoute Signin.State


-- TODO tom: Routes should really be "invertible" so I can create a
-- KeySettings route string from the value.
rootRoutes :: Match RootRoutes
rootRoutes =
  Settings initialState SSHKeySettings <$ lit "settings" <* lit "ssh-keys"
  <|> Settings initialState AccountSettings <$ lit "settings" <* lit "account"
  <|> Settings initialState Profile <$ lit "settings" <* lit "profile"
  <|> Settings initialState EmailSettings <$ lit "settings" <* lit "emails"
  <|> Settings initialState SecuritySettings <$ lit "settings" <* lit "security"
  <|> Settings initialState RepositorySettings <$ lit "settings" <* lit "repositories"
  <|> Settings initialState OrganisationSettings <$ lit "settings" <* lit "organisations"
  <|> pure Route404
