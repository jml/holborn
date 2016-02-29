-- | Holborn permission system. Works as follows: We have an
-- oauth_token where each token is username and password at the same
-- time.
--
-- We have a userFromToken lookup function which returns a set of
-- permissions for a user (if identified). Could be cached at some
-- point but fine for now.

{-# LANGUAGE QuasiQuotes        #-}

module Holborn.Auth
       ( Permission(..)
       , Permissions(..)
       , AuthToken(..)
       , userFromToken
       , hasPermission
       , webPermissions
       , createAuthToken
       , getAuthFromToken
       ) where

import BasicPrelude

import qualified Data.Set as Set
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.Aeson (ToJSON(..), Value(..))
import System.Entropy (getEntropy)
import qualified Data.ByteString.Builder
import Data.ByteString.Lazy (toStrict)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Servant(FromText(..))

import Holborn.Errors (GeneralError(..))
import Holborn.API.Types (AppConf(..))

data AuthToken = AuthToken ByteString deriving Show

data Permission =
      RepoDelete
    | RepoCreate
    | RepoRead
    | OrgRead
    | OrgWrite
    | PublicKeyRead
    | PublicKeyWrite
    | Notifications
    | UserFull
    | UserEmail
    | Web -- special permissions for web users (can do more)
    deriving (Show, Read, Eq, Ord)

-- E.g.
-- read "Permissions (fromList [UserFull])" :: A.Permissions
newtype Permissions = Permissions (Set Permission) deriving (Show, Read)

type UserId = Int


instance FromField Permissions where
    fromField f (Just bs) = return (read (decodeUtf8 bs))

instance ToField AuthToken where
    toField (AuthToken a) = Escape a

instance ToField Permissions where
    toField  = Escape . encodeUtf8 . show

webPermissions :: Permissions
webPermissions = Permissions (Set.fromList [Web])

hasPermission :: Permissions -> Permission -> Bool
hasPermission (Permissions x) p = Set.member p x

userFromToken :: Connection -> AuthToken -> IO (Maybe (UserId, Permissions))
userFromToken conn token = do
    r <- query conn [sql|
        select owner_id, permissions
        from oauth_token where token = ?
        |] (Only token) :: IO [(UserId, Permissions)]

    return $ case r of
      [one] -> Just one
      _ -> Nothing

-- hex-encoded token
createAuthToken :: IO AuthToken
createAuthToken = fmap tok (getEntropy 12)
  where
    tok x = (AuthToken . toStrict . Data.ByteString.Builder.toLazyByteString) (Data.ByteString.Builder.byteStringHex x)


instance ToJSON AuthToken where
    toJSON (AuthToken x) = String (decodeUtf8 x)


-- ExceptT trying to auth the user
getAuthFromToken :: AppConf -> Maybe AuthToken -> ExceptT (GeneralError a) IO (Int, Permissions)
getAuthFromToken AppConf{conn=conn} token = do
    authToken <- case token of
        Nothing -> throwE MissingAuthToken
        Just x -> return x

    maybeUser <- liftIO $ userFromToken conn authToken
    case maybeUser of
        Just (userId, permissions) -> return (userId, permissions)
        Nothing -> throwE InvalidAuthToken


instance FromText AuthToken where
    fromText token = Just (AuthToken (encodeUtf8 token))
