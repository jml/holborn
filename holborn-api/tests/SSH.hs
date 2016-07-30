{-# LANGUAGE QuasiQuotes #-}

module SSH (tests) where

import HolbornPrelude

import Data.Aeson (FromJSON, decode, object, (.=), toJSON, (.:))
import Data.Aeson.Types (Pair, parseMaybe)
import Data.Maybe (fromJust)
import Data.Sequence (fromList)
import Network.Wai.Test (SResponse(..))
import Test.Hspec.Wai (shouldRespondWith, ResponseMatcher(..))
import Test.Hspec.Wai.Internal (withApplication)
import Test.Hspec.Wai.JSON (fromValue, json)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (SpecWith, testSpec, describe, it, shouldBe)
import Web.HttpApiData (toUrlPiece)

import Holborn.API.Config (Config(..))
import Holborn.API.Internal (sql)
import Holborn.JSON.SSHRepoCommunication (parseSSHKey, sshFingerprint)

import Fixtures (makeTestApp, withConfig)
import Helpers
  ( User(..)
  , makeArbitraryUser
  , mutateDB
  , post
  , postAs
  )


tests :: IO TestTree
tests = do
  sshSpec <- testSpec "/v1/ssh" $ withConfig $ spec
  pure $ testGroup "Holborn.API.SSH" [ sshSpec ]


spec :: SpecWith Config
spec = do
  describe "/internal/ssh/authorized-keys" $ do
    it "rejects invalid keys" $ \config -> do
      withApplication (makeTestApp config) $ do
        let badRequest = [json|{key_type: "huh", key: "what?"}|]
        post "/internal/ssh/authorized-keys" badRequest
          `shouldRespondWith` 400

    it "returns empty if there are no matching keys" $ \config -> do
      withApplication (makeTestApp config) $ do
        let req = jsonObj [ "key_type" .= ("RSA" :: Text)
                          , "key" .= validKey
                          ]
        post "/internal/ssh/authorized-keys" req
          `shouldRespondWith` 200 { matchBody = Just [json|[]|] }

    it "includes keys if they are present (RSA)" $ \config -> do
      user <- makeArbitraryUser config
      withApplication (makeTestApp config) $ do
        -- Register an SSH key for the user
        let fullKey = "ssh-rsa " <> validKey <> " comment"
        -- TODO: What's the title for?
        let arbitraryTitle = "test-key" :: Text
        resp <- postAs user "/v1/user/keys" (jsonObj [ "key" .= fullKey
                                                     , "title" .= arbitraryTitle])
        pure resp `shouldRespondWith` 201
        let (Just keyId) = parseMaybe (\obj -> obj .: "id") (getJSONBody resp) :: Maybe Int

        -- Try to request it as an authorized key.
        -- "RSA" here has to match the key type of full key.
        let req = jsonObj [ "key_type" .= ("RSA" :: Text)
                          -- TODO: We have to manually add comment because of
                          -- the way the comparison_pubkey logic works, not
                          -- because this is the desired behaviour of the
                          -- endpoint. Our ssh-authorized-keys binary won't do
                          -- this manual addition (it's never told the comment
                          -- for the key.)
                          , "key" .= (validKey <> " comment")
                          ]
        fingerprint <- liftIO $ fromJust <$> sshFingerprint (encodeUtf8 fullKey)
        let expectedKey = object [ "fingerprint" .= (decodeUtf8 fingerprint)
                                 , "key" .= validKey
                                 ]
        post "/internal/ssh/authorized-keys" req
          `shouldRespondWith` 200 { matchBody = Just (fromValue (toJSON [(keyId, expectedKey)])) }

    it "includes keys if they are present (DSA)" $ \config -> do
      -- TODO: Version that uses DSA keys. Implementation is currently broken.
      withApplication (makeTestApp config) $ do
        pure ()


  describe "/internal/ssh/access-repo" $ do
    it "rejects requests for non-existent keys" $ \config -> do
      withApplication (makeTestApp config) $ do
        let req = jsonObj [ "key_id" .= (1 :: Int)
                          , "command" .= ("git-upload-pack '/no-such-org/no-such-repo'" :: Text)
                          ]
        post "/internal/ssh/access-repo" req
          `shouldRespondWith` 401 { matchBody = Just [json|{"message": "Could not find SSH key"}|] }

    it "rejects requests for non-existent repo" $ \config -> do
      user <- makeArbitraryUser config
      keyId <- makeVerifiedKeyForUser config user
      withApplication (makeTestApp config) $ do
        let req = jsonObj [ "key_id" .= keyId
                          , "command" .= ("git-upload-pack '/no-such-org/no-such-repo'" :: Text)
                          ]
        post "/internal/ssh/access-repo" req
          `shouldRespondWith` 404 { matchBody = Just [json|{"message": "No such repository: no-such-org/no-such-repo"}|] }

    it "routes requests for existent repos" $ \config -> do
      user <- makeArbitraryUser config
      -- Create the key because for reasons jml still doesn't understand, we
      -- need the key ID rather than the user ID in order to decide which
      -- repositories we can access.
      keyId <- makeVerifiedKeyForUser config user
      withApplication (makeTestApp config) $ do
        -- Create a repo for user
        let repoName = "name" :: Text
        resp <- postAs user "/v1/new-repo" $ jsonObj
          [ "owner" .= userName user
          , "name" .= repoName
          , "description" .= ("repo description" :: Text)
          , "private" .= False
          , "initialize" .= False
          ]
        let (Just repoId) = parseMaybe (\obj -> obj .: "id") (getJSONBody resp) :: Maybe Int

        -- Try to get the repo
        let command = "git-upload-pack '/" <> toUrlPiece (userName user) <> "/" <> repoName <> "'"
        let req = jsonObj [ "key_id" .= keyId
                          , "command" .= command
                          ]
        response <- post "/internal/ssh/access-repo" req
        pure response `shouldRespondWith` 200
        let expected = fromList [ toJSON (configRepoHostname config)
                                , toJSON (configRepoPort config)
                                , object [ "command" .= ("GitUploadPack" :: Text)
                                         , "repoId" .= repoId
                                         ]
                                ]
        let observed = getJSONBody response
        liftIO $ observed `shouldBe` expected

    -- Demostrate that we can't get repos without a verified key, and that
    -- there's no real way of verifying.
    it "forbids users logging in with unverified keys" $ \config -> do
      user <- makeArbitraryUser config
      withApplication (makeTestApp config) $ do
        -- Register an SSH key for the user
        let fullKey = "ssh-rsa " <> validKey <> " comment"
        let arbitraryTitle = "test-key" :: Text
        resp <- postAs user "/v1/user/keys" (jsonObj [ "key" .= fullKey
                                                     , "title" .= arbitraryTitle])
        pure resp `shouldRespondWith` 201
        let (Just keyId) = parseMaybe (\obj -> obj .: "id") (getJSONBody resp) :: Maybe Int

        -- Create a repo for user
        let repoName = "name" :: Text
        void $ postAs user "/v1/new-repo" $ jsonObj
          [ "owner" .= userName user
          , "name" .= repoName
          , "description" .= ("repo description" :: Text)
          , "private" .= False
          , "initialize" .= False
          ]

        -- Try to get the repo
        let command = "git-upload-pack '/" <> toUrlPiece (userName user) <> "/" <> repoName <> "'"
        let req = jsonObj [ "key_id" .= keyId
                          , "command" .= command
                          ]
        post "/internal/ssh/access-repo" req `shouldRespondWith` 403



-- TODO: We could probably come up with better helpers for making requests &
-- assertions about responses that always decode to JSON, so we can use AST
-- JSON as a standard type everywhere in tests, rather than going backwards &
-- forwards between bytestring, quasiquotes, and AST.

-- TODO: Is hspec-wai really worth the effort? Could we build better things on
-- top of hunit? or just hspec using stdandard test stuff from wai-extra?

-- TODO: Tests are already getting slow. See if we can restrict ourselves to
-- one holborn database per run.

-- | Create a verified key for a user.
--
-- We don't currently have a supported API (internal or otherwise) for
-- verifying SSH keys, so here we go around the back and create a valid one
-- directly in the database.
--
-- Duplicates quite a lot from Holborn.API.Settings.SSHKeys.addKey.
makeVerifiedKeyForUser :: MonadIO m => Config -> User -> m Int64
makeVerifiedKeyForUser config user = do
  let fullKey = "ssh-rsa " <> validKey <> " comment"
  let (Just sshKey) = parseSSHKey (encodeUtf8 fullKey)
  let arbitraryTitle = "test-key" :: Text
  mutateDB config [sql|
                      insert into "public_key" (id, name, submitted_pubkey, comparison_pubkey, owner_id, verified, readonly, created)
                      values (default, ?, ?, ?, ?, true, false, default)
                      |] (arbitraryTitle, fullKey, sshKey, (userId user))


jsonObj :: [Pair] -> LByteString
jsonObj = fromValue . object


validKey :: Text
validKey = "AAAAB3NzaC1yc2EAAAADAQABAAABAQClEBRSTpSY668/c66R1QC/c1VUGmVBH4uxjbp8KNiF97VT3Dgl2b6gi/5KV5bCNpF4GqsrohzrEI3mf5XLF01ZIcW0pFUZfasWoS+7sQqJnFkb+fjbJDWo62YrLyJyQz7xXvqiUbm87cFA2zWy13jOoxAId5XxdCIEVQt3j4YdbbzFwGayBwgkBtvfok0geJgaTf2j8eRKvdb99ZFsRUlkyO/vqo/cjxXKbfpN3usykQ19To6k9MRANvWryhvAgth2qp4p8F7Zf2w3ZMly3POF8tZBiVE9QdE6QH860aBA8GNjTBCNRGrULilmh4IuXOdAhxSUR1RlDlHuKv0VrKBh"


getJSONBody :: (FromJSON a) => SResponse -> a
getJSONBody = fromJust . decode . simpleBody
