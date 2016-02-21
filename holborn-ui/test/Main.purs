module Test.Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Generic
import Data.Argonaut.Parser as P
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Either.Unsafe (fromRight)
import Data.Argonaut.Core (Json(), isNull, foldJsonNull, foldJsonBoolean, foldJsonNumber, foldJsonString, toArray, toNumber, toObject, toString, toBoolean)
import Control.Bind ((=<<))
import Data.Int (fromNumber)
import Data.String (charAt, toChar)
import Data.Traversable (traverse, for)
import Data.StrMap as M
import Data.Foldable (find)
import Data.Array (zipWithA, length, head)

import Test.Unit (test, runTest, TestUnit)
import Test.Unit.Assert as Assert
import Data.Maybe (Maybe(..), maybe)
import Type.Proxy (Proxy(..))
import Control.Monad.Aff.AVar (AVAR)



import Debug.Trace

-- Adapted from
-- https://github.com/purescript-contrib/purescript-argonaut-codecs/blob/master/src/Data/Argonaut/Decode.purs
-- so MIT licensed.
--
-- The adaption is to remove the `tag` requirement when reading. We
-- accept any type if it decodes.
gDecode' :: GenericSignature -> Json -> Either String GenericSpine
gDecode' sig json = case sig of
    SigNumber -> SNumber <$> mFail "Expected a number" (toNumber json)
    SigInt -> SInt <$> mFail "Expected an integer number" (fromNumber =<< toNumber json)
    SigString -> SString <$> mFail "Expected a string" (toString json)
    SigChar -> SChar <$> mFail "Expected a char" (toChar =<< toString json)
    SigBoolean -> SBoolean <$> mFail "Expected a boolean" (toBoolean json)
    SigArray thunk -> do
      jArr <- mFail "Expected an array" $ toArray json
      SArray <$> traverse (map const <<< gDecode' (thunk unit)) jArr

    SigRecord props -> do
      jObj <- mFail "Expected an object" $ toObject json
      SRecord <$> for props \({recLabel: lbl, recValue: val}) -> do
        pf <- mFail ("'" <> lbl <> "' property missing") (M.lookup lbl jObj)
        sp <- gDecode' (val unit) pf
        pure { recLabel: lbl, recValue: const sp }

    SigProd typeConstr alts -> do
      let decodingErr msg = "When decoding a " ++ typeConstr ++ ": " ++ msg

      _ <- case length alts of
        1 -> Right unit
        _ -> Left (decodingErr ("Must have exactly one data constructor."))

      tp <- case head alts of
        Nothing -> Left "uhh"
        Just x -> Right x

      sps  <- zipWithA (\k -> gDecode' (k unit)) tp.sigValues [json]
      sp <- case head sps of
        Nothing -> Left "ahh"
        Just x -> Right x

      pure $ SProd tp.sigConstructor [\_ -> sp]
  where
    mFail :: forall a. String -> Maybe a -> Either String a
    mFail msg = maybe (Left msg) Right


gDecode :: forall a. (Generic a) => Json -> Either String a
gDecode json = maybe (Left "fromSpine failed") Right <<< fromSpine
               =<< gDecode' (toSignature (Proxy :: Proxy a)) json

-- Need eq instance for test
newtype A = A { name :: String }
instance eqA :: Eq A where
  eq (A a) (A b) = eq a.name b.name
derive instance genericA :: Generic A

data B = B1 { name :: String } | B2
instance eqB :: Eq B where
  eq _ _ = false
derive instance genericB :: Generic B

testJson = fromRight (P.jsonParser "{\"name\": \"name\"}")


main = runTest do
  test "gDecode" do
    Assert.assert "expect success" $
      (gDecode testJson == Right (A {name: "name"}) :: Either String A)
    Assert.assert "expect broken data constructor" $
      (gDecode testJson == Left "When decoding a Test.Main.B: Must have exactly one data constructor."  :: Either String B)
