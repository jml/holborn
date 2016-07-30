{-# LANGUAGE QuasiQuotes #-}

-- | Code for manipulating holborn-api that does not actually belong in
-- holborn-api.

module Helpers
  ( User(..)
  , getJSONBody
  , makeArbitraryUser
  , mutateDB
  , post
  , postAs
  ) where

import HolbornPrelude

import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (FromJSON, decode)
import Data.Maybe (fromJust)
import Database.PostgreSQL.Simple (Query, ToRow)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Method (methodPost)
import Test.Hspec.Wai (WaiSession, request)
import Network.Wai.Test (SResponse(..))
import Web.HttpApiData (toHeader)

import Holborn.API.Auth (UserId)
import Holborn.API.Config (Config)
import Holborn.API.Internal
  ( execute
  , runAPIHandler
  , sql
  )
import Holborn.API.Types
  ( Email
  , Username
  , newUsername
  , newEmail
  )

-- | A user we have created for testing.
--
-- Exists so we can provide easy APIs for writing tests without worrying too
-- much about impact on production code.
data User = User { userId    :: UserId
                 , userName  :: Username
                 , _userEmail :: Email
                 } deriving (Eq, Show)


mutateDB :: (ToRow args, Show args, MonadIO m) => Config -> Query -> args -> m Int64
mutateDB config query params = do
  result <- liftIO $ runExceptT $ runAPIHandler config $ execute query params
  case result of
    Left _ -> terror $ "Error running query: " <> show query <> " " <> show params
    Right r -> pure r

-- | Make an arbitrary user for testing.
makeArbitraryUser :: MonadIO m => Config -> m User
makeArbitraryUser config = do
  -- TODO: Make this actually arbitrary.
  -- TODO: Remove duplication between query & `User` construction.
  userid <- mutateDB config [sql|insert into "user" (username, email) values (?, ?)|] ("alice" :: Text, "alice@example.com" :: Text)
  pure $ User (fromIntegral userid) username email
  where
    username = newUsername "alice"
    email = fromJust (newEmail "alice@example.com")

-- | Post JSON to 'path' anonymously.
post :: ByteString -> LByteString -> WaiSession SResponse
post path body = request methodPost path [jsonContent] body

-- | Post JSON to 'path' as the given user.
postAs :: User -> ByteString -> LByteString -> WaiSession SResponse
postAs user path body = request methodPost path [authHeader user, jsonContent] body

jsonContent :: (HeaderName, ByteString)
jsonContent = ("content-type", "application/json")

authHeader :: User -> (HeaderName, ByteString)
authHeader user = ("GAP-Auth", (toHeader (userName user)))

getJSONBody :: (FromJSON a) => SResponse -> a
getJSONBody = fromJust . decode . simpleBody
