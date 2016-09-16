{-# LANGUAGE QuasiQuotes #-}

-- | Code for manipulating holborn-api that does not actually belong in
-- holborn-api.

module Helpers
  ( User(..)
  , makeArbitraryUser
  , getJSONBody
  , respondsWithJSON
  , mutateDB
  , get
  , post
  , postAs
  ) where

import HolbornPrelude

import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Maybe (fromJust)
import Database.PostgreSQL.Simple (Query, ToRow)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Method (methodGet, methodPost)
import Network.Wai.Test (SResponse(..))
import Test.Hspec.Wai (WaiSession, request, shouldRespondWith)
import Test.Tasty.Hspec (shouldBe)
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
  userid <- mutateDB config [sql|
      insert into auth_user (password, is_superuser, username, first_name, last_name, email, is_staff, is_active, date_joined)
      values ('', false, ?, '', '', ?, false, true, now() at time zone 'utc')|] ("alice" :: Text, "alice@example.com" :: Text)
  pure $ User (fromIntegral userid) username email
  where
    username = newUsername "alice"
    email = fromJust (newEmail "alice@example.com")

-- | Get JSON at 'path' anonymously.
get :: Text -> WaiSession SResponse
get path = request methodGet (encodeUtf8 path) [jsonContent] ""

-- | Post JSON to 'path' anonymously.
post :: ToJSON a => Text -> a -> WaiSession SResponse
post path body = request methodPost (encodeUtf8 path) [jsonContent] (encode body)

-- | Post JSON to 'path' as the given user.
postAs :: ToJSON a => User -> Text -> a -> WaiSession SResponse
postAs user path body = request methodPost (encodeUtf8 path) (jsonContent:(dexHeaders user)) (encode body)

jsonContent :: (HeaderName, ByteString)
jsonContent = ("content-type", "application/json")

dexHeaders :: User -> [(HeaderName, ByteString)]
dexHeaders user =
  [ ("x-dex-name", (toHeader (userName user)))
  , ("x-dex-email", (toHeader (_userEmail user)))
  , ("x-dex-email-verified", (toHeader True))
  ]

getJSONBody :: (FromJSON a) => SResponse -> a
getJSONBody = fromJust . decode . simpleBody

respondsWithJSON :: (Show a, Eq a, FromJSON a) => WaiSession SResponse -> a -> WaiSession ()
respondsWithJSON action expected = do
  response <- action
  pure response `shouldRespondWith` 200
  liftIO $ getJSONBody response `shouldBe` expected
