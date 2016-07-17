{-# LANGUAGE DeriveGeneric #-}

module Holborn.Proxy.AuthJar
  ( TrustedCreds
  , MemoryJar
  , AuthJar(..)
  , newMemoryJar
  , UserCookie
  , unpackClaims
  , trustedCredsHeaders
  ) where


import HolbornPrelude

import qualified Data.HashMap.Strict as HashMap
import Control.Concurrent.STM.TVar (newTVarIO, modifyTVar', readTVarIO, readTVar, TVar)
import Control.Concurrent.STM (atomically)
import GHC.Generics (Generic)
import System.Entropy (getEntropy)
import Data.ByteString.Base64 (encode)
import Network.OAuth.OAuth2 (AccessToken(idToken))
import Network.HTTP.Types.Header (Header)
import qualified Data.ByteString.Lazy as BSL
import Crypto.JWT (JWT(..), ClaimsSet(_unregisteredClaims))
import Data.Aeson (fromJSON, Result(..))
import Crypto.JOSE (decodeCompact)


-- | TrustedCreds are extracted from the id_token returned by
-- fetchAccessToken and are therefore trustworthy.
data TrustedCreds = TrustedCreds
  { email :: Text
  , emailVerified :: Bool
  , name :: Text
  } deriving (Show, Generic)

instance Hashable TrustedCreds

type UserCookie = ByteString

type CookieMap = HashMap.HashMap UserCookie TrustedCreds


-- | MemoryJar is keeping auth cookies in local memory, not on
-- disk. This means they will be lost on restart.
data MemoryJar = MemoryJar (TVar CookieMap)

newMemoryJar :: IO MemoryJar
newMemoryJar = MemoryJar <$> newTVarIO HashMap.empty

class AuthJar a where
    get :: a -> UserCookie -> IO (Maybe TrustedCreds)
    set :: a -> UserCookie -> TrustedCreds -> IO ()
    make :: a -> IO UserCookie


-- | An in-memory jar for testing
instance AuthJar MemoryJar where
    set (MemoryJar jar) key token = atomically (modifyTVar' jar (HashMap.insert key token))
    get (MemoryJar jar) key = atomically (readTVar jar) >>= \m -> pure (HashMap.lookup key m)
    make _ = fmap encode (getEntropy 128)


-- TODO: unpackClaims is nested too deeply but checked by
-- compiler and no IO so I assume it works..
unpackClaims :: AccessToken -> Either String TrustedCreds
unpackClaims token =
  case idToken token of
    Just id' ->
        -- PUPPY I don't *think* we need to verify the JWT
        -- access token because it's been given to us by
        -- dex. Might be wrong though. If we do need to check we
        -- need to send a verify request as well and use the
        -- keys from the response to verify the token.
        case decodeCompact (BSL.fromStrict id') of
          Right jwt ->
            case emailAndNameClaims (jwtClaimsSet (jwt :: JWT)) of
              Nothing -> Left (textToString ("missing claims. Token: " <> (show jwt)))
              Just claims ->
                case claims of
                  Success creds -> Right creds
                  Error err -> Left err
          _ -> Left "Could not decode claims"
    Nothing -> Left "missing id_token"


emailAndNameClaims claimsSet = do
    let c = _unregisteredClaims claimsSet
    email <- fmap fromJSON (HashMap.lookup "email" c)
    email_verified <- (fmap fromJSON (HashMap.lookup "email_verified" c)) <|> Just (Success False)
    name <- fmap fromJSON (HashMap.lookup "name" c)
    pure $ TrustedCreds <$> email <*> email_verified <*> name


trustedCredsHeaders :: TrustedCreds -> [Header]
trustedCredsHeaders TrustedCreds{..} =
  [ ("x-holborn-name", encodeUtf8 name)
  , ("x-holborn-email", encodeUtf8 email)
  , ("x-holborn-email-verified", encodeUtf8  (show emailVerified))
  ]
