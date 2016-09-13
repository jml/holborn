module Test.Main where

import Prelude
import Data.Generic (class Generic, gShow)
import Data.Argonaut.Parser as P
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Test.Unit (test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Data.Either (fromRight)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe(..))
import Data.Argonaut.Printer (printJson)

import Holborn.JSON.Generic (gDecode, gEncode)
import Holborn.ManualEncoding.CreateRepository (Visibility(..))


-- Need eq instance for test
newtype A = A { name :: String }
instance eqA :: Eq A where
  eq (A a) (A b) = eq a.name b.name
derive instance genericA :: Generic A
instance showA :: Show A where show = gShow

-- Can't decode types with multiple data constructors:
data B = B1 { name :: String } | B2
instance eqB :: Eq B where
  eq _ _ = false
derive instance genericB :: Generic B
instance showB :: Show B where show = gShow

-- Handle maybe:
data MA = MA { name :: Maybe String }
instance eqMA :: Eq MA where
  eq (MA a) (MA b) = eq a.name b.name
derive instance genericMA :: Generic MA
instance showMA :: Show MA where show = gShow

-- Handle maybe with subtype:
data MAV = MAV { visibility :: Maybe Visibility }
instance eqMAV :: Eq MAV where
  eq (MAV a) (MAV b) = eq a.visibility b.visibility
derive instance genericMAV :: Generic MAV
instance showMAV :: Show MAV where show = gShow


testJson :: Json
testJson = unsafePartial (fromRight (P.jsonParser "{\"name\": \"name\"}"))
emptyJson :: Json
emptyJson = unsafePartial (fromRight (P.jsonParser "{}"))


main = runTest do
  test "gDecode valid case" do
    Assert.equal (gDecode testJson) (Right (A {name: "name"}))
  test "gDecode too many constructor branches" do
    Assert.equal (gDecode testJson) (Left "When decoding a Test.Main.B: Must have exactly one data constructor."  :: Either String B)
  test "gDecode maybe empty JSON should be Nothing" $
    Assert.equal (Right (MA {name: Nothing})) (gDecode emptyJson)
  test "gDecode maybe valid JSON should be Just" $
    Assert.equal (Right (MA {name: Just "name"})) (gDecode testJson)
  test "gEncode doesn't encode tag" $
    Assert.equal "{\"name\":\"name\"}" (printJson (gEncode (A {name: "name"})))
  test "gEncode maybe valid Visibility must not be null" $
    Assert.equal "{\"visibility\":\"public\"}" (printJson (gEncode (MAV {visibility: Just Public})))
