-- | Holborn permission system. Works as follows: We have an
-- oauth_token where each token is username and password at the same
-- time.
--
-- We have a userFromToken lookup function which returns a set of
-- permissions for a user (if identified). Could be cached at some
-- point but fine for now.

module Holborn.Auth
       ( Permission(..)
       , Permissions(..)
       , hasPermission
       , webPermissions
       ) where

import BasicPrelude

import qualified Data.Set as Set
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..))


-- Borrowed fom the github permission system. Not super clear what
-- each one entails ATM.
data Permission =
      RepoDelete
    | RepoCreate
    | RepoRead
    | RepoPush
    | OrgRead
    | OrgWrite
    | PublicKeyRead
    | PublicKeyWrite
    | Notifications
    | UserFull
    | UserEmail
    | Web -- special permissions for web users (e.g. Google Chrome or
          -- authenticated CURL). This should really be several
          -- permissions down the line but easier for now to have a
          -- catch-all.
    deriving (Show, Read, Eq, Ord)

-- E.g.
-- read "Permissions (fromList [UserFull])" :: A.Permissions
newtype Permissions = Permissions (Set Permission) deriving (Show, Read)


-- | Function to better communicate what's going wrong:
dataCorruptError :: Text -> a
dataCorruptError = terror


instance FromField Permissions where
    fromField _ (Just bs) = case readMay (decodeUtf8 bs) of
        Just p -> pure p
        Nothing -> dataCorruptError
          ("Could not parse permissions, probably due to changed data type. Update oauth_token table: " <> (decodeUtf8 bs))
    fromField _ Nothing = terror "FromField Permissions should always decode correctly"


instance ToField Permissions where
    toField  = Escape . encodeUtf8 . show


webPermissions :: Permissions
webPermissions = Permissions (Set.fromList [Web])

hasPermission :: Permissions -> Permission -> Bool
hasPermission (Permissions x) p = Set.member p x
