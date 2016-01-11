{-# LANGUAGE RankNTypes #-}
import BasicPrelude
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit
import Holborn.API.Types (newPassword, newUsername, newEmail)

import Database.PostgreSQL.Simple
import Control.Error
import Holborn.API.User (listUsers, signup)
import Test.QuickCheck


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests =
  testGroup "Holborn.API"
  [ testCase "password not shown" $ do
        pwd <- newPassword "hello"
        show pwd @?= "*hidden-password*"
  ]


db = do
    c <- connect (defaultConnectInfo  { connectDatabase = "holborn-test", connectUser = "tom"})
    pwd <- newPassword "password" -- password is in IO so can't generate in an arbitray
    let fns = [ ((listUsers c) <$> arbitrary)
--             , ((signup c) <$> arbitrary <*> arbitrary <*> (pure pwd)) >> return ()
              ] :: (forall a. Gen (ExceptT ApiError )
    return $ fmap runExceptT (oneof fns)
