module Holborn.Routing where

import Prelude

import Data.Functor ((<$))
import Control.Apply ((<*))
import Control.Alt ((<|>))
import Routing.Match (Match)
import Routing.Match.Class (lit)
import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Network.HTTP.Affjax as AJ
import Data.List (List)
import Holborn.ManualEncoding.Keys (Key)
import Data.Argonaut.Decode (decodeJson)


baseURL = "http://127.0.0.1:8002/v1"

-- We have one route to select where we want to go, and an "OK" route
-- for when the data fetch was successful.
data SettingsRoutes =
    SSHKeySettings
  | SSHKeySettingsOK (List Key)
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


data RootRoutes =
    EmptyRoute
  | Settings SettingsRoutes
  | Route404
  | ErrorRoute
  | SigninRoute


-- TODO tom: Routes should really be "invertible" so I can create a
-- KeySettings route string from the value.
rootRoutes :: Match RootRoutes
rootRoutes =
  Settings SSHKeySettings <$ lit "settings" <* lit "ssh-keys"
  <|> Settings AccountSettings <$ lit "settings" <* lit "account"
  <|> Settings Profile <$ lit "settings" <* lit "profile"
  <|> Settings EmailSettings <$ lit "settings" <* lit "emails"
  <|> Settings SecuritySettings <$ lit "settings" <* lit "security"
  <|> Settings RepositorySettings <$ lit "settings" <* lit "repositories"
  <|> Settings OrganisationSettings <$ lit "settings" <* lit "organisations"
  <|> pure Route404


fetchData :: forall eff. RootRoutes -> Aff (ajax :: AJ.AJAX | eff) RootRoutes
fetchData (Settings SSHKeySettings) = do
  r <- AJ.get (baseURL ++ "/users/tom/keys")
  return $ case decodeJson r.response of
    Left err -> ErrorRoute
    Right keys -> Settings (SSHKeySettingsOK keys)


-- General case: Return route unmodified
fetchData x = return x
