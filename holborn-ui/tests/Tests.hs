import BasicPrelude
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit
import Holborn.UI.Types (newPassword)

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests =
  testGroup "Holborn UI"
  [ testCase "password not shown" $ do
        pwd <- newPassword "hello"
        show pwd @?= "*hidden-password*"
  ]
