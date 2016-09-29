-- | Tests for Holborn.API.Settings.SSHKeys

module Settings.SSHKeys (spec) where

import HolbornPrelude hiding (get)

import Data.Aeson (FromJSON, Object, object, (.=), (.:))
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromJust)
import Test.Tasty.Hspec (SpecWith, describe, it, shouldBe)
import Test.Hspec.Wai (shouldRespondWith)
import Test.Hspec.Wai.Internal (withApplication)
import Web.HttpApiData (toUrlPiece)

import Holborn.API.Config (Config)
import Holborn.JSON.SSHRepoCommunication (SSHKey(SSHKey), parseSSHKey)

import Fixtures (makeTestApp)
import Helpers
  ( User(..)
  , get
  , getJSONBody
  , makeArbitraryUser
  , post
  , postAs
  , respondsWithJSON
  )

spec :: SpecWith Config
spec = do
  describe "/v1/users/<username>/keys" $ do
    it "Returns empty list for non-existent users" $ \config -> do
      withApplication (makeTestApp config) $ do
        -- TODO: Actually, a 404 would be more appropriate, but might as well
        -- test current behaviour.
        get "/v1/users/suki/keys" `respondsWithJSON` emptyList

    it "Returns empty list for users without keys" $ \config -> do
      user <- makeArbitraryUser config
      withApplication (makeTestApp config) $ do
        get ("/v1/users/" <> toUrlPiece (userName user) <> "/keys") `respondsWithJSON` emptyList

    it "Lists keys you have added" $ \config -> do
      user <- makeArbitraryUser config
      withApplication (makeTestApp config) $ do
        let req = object [ "key" .= (decodeUtf8 validKey) ]
        response <- postAs user "/v1/user/keys" req
        let addedKey :: Object = getJSONBody response
        get ("/v1/users/" <> toUrlPiece (userName user) <> "/keys")
          `respondsWithJSON` [addedKey]

  describe "/v1/user/keys" $ do
    it "Forbids anonymous users to add keys" $ \config -> do
      withApplication (makeTestApp config) $ do
        let req = object [ "key" .= (decodeUtf8 validKey)
                         ]
        post "/v1/user/keys" req `shouldRespondWith` 401

    it "400s when users try to add invalid keys" $ \config -> do
      user <- makeArbitraryUser config
      withApplication (makeTestApp config) $ do
        let req = object [ "key" .= ("not a valid SSH key" :: Text)
                         ]
        postAs user "/v1/user/keys" req `shouldRespondWith` 400

    it "Lets you add an SSH key" $ \config -> do
      user <- makeArbitraryUser config
      withApplication (makeTestApp config) $ do
        let req = object [ "key" .= (decodeUtf8 validKey) ]
        response <- postAs user "/v1/user/keys" req
        pure response `shouldRespondWith` 201
        let observed = getJSONBody response
        let (Just (SSHKey keyType keyData comment)) = parseSSHKey validKey
        liftIO $ (observed `getKey` "verified") `shouldBe` False
        liftIO $ (observed `getKey` "readonly") `shouldBe` True
        liftIO $ (observed `getKey` "id") `shouldBe` (1 :: Int)
        liftIO $ (observed `getKey` "key") `shouldBe`
          object [ "type" .= keyType
                 , "key" .= decodeUtf8 keyData
                 , "comment" .= (decodeUtf8 <$> comment)
                 ]

    it "Lets you add duplicate keys" $ \config -> do
      user <- makeArbitraryUser config
      withApplication (makeTestApp config) $ do
        let req = object [ "key" .= (decodeUtf8 validKey) ]
        postAs user "/v1/user/keys" req `shouldRespondWith` 201
        postAs user "/v1/user/keys" req `shouldRespondWith` 201

getKey :: FromJSON b => Object -> Text -> b
getKey obj key = fromJust (parseMaybe (\x -> x .: key) obj)

-- | An empty list.
--
-- We don't care what the type is, just as long as it's something that
-- implements Eq.
emptyList :: [Int]
emptyList = []


-- | A valid SSH key
validKey :: ByteString
validKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDQJqij/IOZ+j7gcv5eTo+buu48viZnKiVEKY7mOzdIVm+UBRB+f0YY3F9jKCNqwOOyXEvn6a/PieD6m5Xax33L8ZChhopbhq/XWlMmpYZz+jmwLRwBRH3ZJWUygsLenJM0PoLUEiZ4KkX+tH+ByBYDWJJtMANrZLPd6L3Aqr+TTNaAfHI868w2cFcEpMkiFRP7m1ksfaFYOkxrH8fd8aKfQvo+/jmaJQ2QGIfUIhpe2kVi/gzIAbzu0GU2XngNWoHurpwfT7CdJ3Bc/uCEmS34HImfMsGoNa62/pvI2KyssTOuqkmFgNohnO9SOFO4u+sRqtRfBPcO/OldBVnjTMXL jml@worth"
