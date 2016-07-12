{-# LANGUAGE DeriveGeneric #-}

module Holborn.Proxy.AuthJar
  ( TrustedCreds(..)
  , MemoryJar
  , AuthJar(..)
  , newMemoryJar
  , UserCookie
  ) where


import HolbornPrelude

import qualified Data.HashMap.Strict as HashMap
import Control.Concurrent.STM.TVar (newTVarIO, modifyTVar', readTVarIO, readTVar, TVar)
import Control.Concurrent.STM (atomically)
import GHC.Generics (Generic)
import System.Entropy (getEntropy)
import Data.ByteString.Base64 (encode)


-- | TrustedCreds are extracted from the id_token returned by
-- fetchAccessToken and are therefore turstworthy.
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
    make _ = fmap encode (getEntropy 2048)
