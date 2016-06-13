{-# LANGUAGE DeriveGeneric #-}

module Holborn.Proxy.AuthJar
  ( TrustedCreds(..)
  , MemoryJar
  , AuthJar(..)
  , newMemoryJar
  , UserCookie
  ) where


import BasicPrelude
import qualified Data.HashMap.Strict as HashMap
import Control.Concurrent.STM.TVar (newTVarIO, modifyTVar', readTVarIO, TVar)
import Control.Concurrent.STM (atomically)
import GHC.Generics (Generic)

-- | TrustedCreds are extracted from the id_token returend by
-- fetchAccessToken and are therefore turstworthy.
data TrustedCreds = TrustedCreds
  { _email :: Text
  , _emailVerified :: Bool
  , _name :: Text
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
    get (MemoryJar jar) key = readTVarIO jar >>= \m -> pure (HashMap.lookup key m)
    make _ = pure "test-cookie" -- TODO random bytes
