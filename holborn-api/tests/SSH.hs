{-# LANGUAGE QuasiQuotes #-}

module SSH (tests) where

import HolbornPrelude

import Data.Aeson (FromJSON, decode, object, (.=), toJSON, (.:))
import Data.Aeson.Types (Pair, parseMaybe)
import Data.Maybe (fromJust)
import Data.Sequence (fromList)
import Network.Wai.Test (SResponse(..))
import Test.Hspec.Wai (shouldRespondWith, ResponseMatcher(..), WaiSession)
import Test.Hspec.Wai.Internal (withApplication)
import Test.Hspec.Wai.JSON (fromValue, json)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (SpecWith, testSpec, describe, it, shouldBe)
import Web.HttpApiData (toUrlPiece)

import Holborn.API.Config (Config(..))
import Holborn.API.Internal (sql)
import Holborn.JSON.SSHRepoCommunication (SSHKey(SSHKey), parseSSHKey)

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
        let (Just (SSHKey keyType key _ _)) = parseSSHKey exampleKey
        let req = jsonObj [ "key_type" .= toJSON keyType
                          , "key" .= decodeUtf8 key
                          ]
        post "/internal/ssh/authorized-keys" req
          `shouldRespondWith` 200 { matchBody = Just [json|[]|] }

    it "includes keys if they are present (RSA)" $ \config -> do
      user <- makeArbitraryUser config
      withApplication (makeTestApp config) $ do
        submitAndFetchKey user rsaKey "arbitrary title"

    it "includes keys if they are present (DSA)" $ \config -> do
      user <- makeArbitraryUser config
      withApplication (makeTestApp config) $ do
        submitAndFetchKey user dsaKey "arbitrary title"

    it "includes keys if they are present (no comment)" $ \config -> do
      user <- makeArbitraryUser config
      withApplication (makeTestApp config) $ do
        submitAndFetchKey user keyWithoutComment "arbitrary title"

    it "includes keys if they are present (complex comment)" $ \config -> do
      user <- makeArbitraryUser config
      withApplication (makeTestApp config) $ do
        submitAndFetchKey user keyWithComplexComment "arbitrary title"

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
        let arbitraryTitle = "test-key" :: Text
        resp <- postAs user "/v1/user/keys" (jsonObj [ "key" .= decodeUtf8 exampleKey
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
  let (Just sshKey) = parseSSHKey exampleKey
  let arbitraryTitle = "test-key" :: Text
  mutateDB config [sql|
                      insert into "public_key" (id, name, submitted_pubkey, comparison_pubkey, owner_id, verified, readonly, created)
                      values (default, ?, ?, ?, ?, true, false, default)
                      |] (arbitraryTitle, exampleKey, sshKey, (userId user))


jsonObj :: [Pair] -> LByteString
jsonObj = fromValue . object


rsaKey, dsaKey, keyWithoutComment, keyWithComplexComment, exampleKey :: ByteString
rsaKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDQJqij/IOZ+j7gcv5eTo+buu48viZnKiVEKY7mOzdIVm+UBRB+f0YY3F9jKCNqwOOyXEvn6a/PieD6m5Xax33L8ZChhopbhq/XWlMmpYZz+jmwLRwBRH3ZJWUygsLenJM0PoLUEiZ4KkX+tH+ByBYDWJJtMANrZLPd6L3Aqr+TTNaAfHI868w2cFcEpMkiFRP7m1ksfaFYOkxrH8fd8aKfQvo+/jmaJQ2QGIfUIhpe2kVi/gzIAbzu0GU2XngNWoHurpwfT7CdJ3Bc/uCEmS34HImfMsGoNa62/pvI2KyssTOuqkmFgNohnO9SOFO4u+sRqtRfBPcO/OldBVnjTMXL jml@worth"
dsaKey = "ssh-dss AAAAB3NzaC1kc3MAAACBAP4XolT62nkT7tWiQ2d9Cv35s6JSN4PvYLPupmLhHlC1D+Q5K2yAwvphFP8XEr+42BxG/fY/aZG8hvo4HzAzYT4llSzJQRzfqznJyHGB5ZJ/Pk80uJir9LKUlh5DXjl+h3KtCpByMJk5ewRtW3Q1yBFX0xTRyPWlpQZW90mwlhHpAAAAFQCvOOTfFYDywM6Tu945PFoVqZlZxQAAAIBBz5tLladTqFElbPXzeCURn47FlvIOrL1F1WTEdAQ1ApZzuOD14asZ6DLA5eAktjFeZbsYx8wsTX6lHhNbE9sC2KNVa1P4YdTX7E3REIlf2/Rt9JShkPXaV7exxi8E5qaYHC7zjaQQhuciANd79WTgdgEY+0+G8erWROqEQiBUegAAAIBaJVw0OhDWtQt9z8V7efS/VITCJzyO6tS9kJww8mDxZTdjU+z/DRWaxnCp1LW06NI7+PXLS3aDnu4eU3RkHPi6EAPVvM0te/mKNVpjDjzYlkbDhawY2585DGJDyvGXF1FK/21xq+iZpjUSYsrrZ6bc0ynaqZ43Gi/EyBr+aY5Yng== jml@worth"
keyWithoutComment = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDA9TbBf4tGNPtAAKPwzyKudBSnOFqen+sToJHBysC+ERYmy6CVLVYitW/ZzxkAmooXZrPGh+VBoa6hFy5o5CMQUoCJQpIbORuyWIyeAplI5CHMFEBB8yRyfLXdpVJ9iBhNlpz0PR9PhCxyrEJc4SvyNd9Iu5DaonLxV7brplcZ9Z54UPaMfiDpn5kUr0fmKGiVCi2NsHzES0Nh6lwRKVVzYIEzbFx8qQZmQbk0oeexOYxf1LkYq6M3MOABSzHMrty3kuQlZpusQz4/jTOVBSv3iLzk4WCcLjKtnggWF0QhcjKP94xslzjgw6AAdauUsJ6DMfKHIRdyMUOiOQ5/YCz7"
keyWithComplexComment = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDiNWY5vRUTCJcaFhJ/CX+BWty1buVO1Qnhm79sTk2Dchc8cpFfsBVyps5rjmcfiMVRayTfnVAe6HcXUA0kMKSsyBth8Xl0WFJtkDlWeJ2PTbLC+g+GHyuM256GwNXZKJ5ZoYdee2UPvomyUYogiJLrfvrcWd+AM7i4xWYYFriJGRF6tugyiPy/2rCZc4yn8EcTwORp3z7DY5b0QWdPonwhxXvjNkAoKU+mM8o12inZJrKY0z8JSgrJVFVPe1sdT3V48LFGYjVzOYH6ycV7eWvzg2XhZl1XyyVrscrW9Ew+I01UFdHo6tS3n6+AYCTFU7fuFA3yOr7IXEsCvpBfgBqF I am not a number, I'm a free man"
exampleKey = rsaKey  -- For when we don't care about the particular properties of the key


getJSONBody :: (FromJSON a) => SResponse -> a
getJSONBody = fromJust . decode . simpleBody


-- | A key which is submitted by a user appears in their authorized keys list.
--
-- Note that this happens regardless of whether the key has been 'verified' or
-- not (a concept only partially implemented within holborn-api).
--
-- When we fully implement SSH key verification, this property should be
-- updated to verify the key and show that it is then present in authorized keys.
submitAndFetchKey :: User -> ByteString -> Text -> WaiSession ()
submitAndFetchKey user fullKey title = do
  let (Just parsedKey) = parseSSHKey fullKey
  let (SSHKey keyType key comment fingerprint) = parsedKey
  resp <- postAs user "/v1/user/keys" (jsonObj [ "key" .= decodeUtf8 fullKey
                                               , "title" .= title
                                               ])
  pure resp `shouldRespondWith` 201
  let (Just keyId) = parseMaybe (\obj -> obj .: "id") (getJSONBody resp) :: Maybe Int

  -- Try to request it as an authorized key.
  let req = jsonObj [ "key_type" .= toJSON keyType
                      -- TODO: We have to manually add comment because of
                      -- the way the comparison_pubkey logic works, not
                      -- because this is the desired behaviour of the
                      -- endpoint. Our ssh-authorized-keys binary won't do
                      -- this manual addition (it's never told the comment
                      -- for the key.)
                    , "key" .= decodeUtf8 (key <> maybe mempty (" " <>) comment)
                    ]
  let expectedKey = object [ "fingerprint" .= decodeUtf8 fingerprint
                           , "key" .= decodeUtf8 key
                           , "type" .= toJSON keyType
                           , "comment" .= (decodeUtf8 <$> comment)
                           ]

  post "/internal/ssh/authorized-keys" req
    `shouldRespondWith` 200 { matchBody = Just (fromValue (toJSON [(keyId, expectedKey)])) }
