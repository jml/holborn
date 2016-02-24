-- Adapted from
-- https://github.com/purescript-contrib/purescript-argonaut-codecs/blob/master/src/Data/Argonaut/Decode.purs
-- so MIT licensed.
--
-- This version differs from the version above in two ways:
-- * We only deal with single data-constructor types (e.g. newtype)
-- * We automatically handle Maybe as optionl (entry missing -> Nothing)
module Holborn.JSON.Generic where

import Prelude ((<<<), pure, ($), bind, unit, (==), (++), const, (<>), (<$>), map)

import Data.Generic (class Generic, GenericSignature(..), GenericSpine(..), toSignature, fromSpine, toSpine)

import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Argonaut.Core (Json, toObject, toArray, toBoolean, toString, toNumber)

import Control.Bind ((=<<))
import Data.Int (fromNumber)
import Data.String (toChar)
import Data.Traversable (traverse, for)
import Data.StrMap as M
import Data.Array (zipWithA, length, head)

import Data.Maybe (Maybe(..), maybe)
import Type.Proxy (Proxy(..))
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)


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
        case val unit of
          constr@(SigProd "Data.Maybe.Maybe" sigValues) -> case M.lookup lbl jObj of
            Nothing -> pure { recLabel: lbl, recValue: \_ -> toSpine (Nothing :: Maybe Int) }
            Just pf -> do
              sp <- gDecode' constr pf
              pure { recLabel: lbl, recValue: const sp }
          _ -> do
            pf <- mFail ("'" <> lbl <> "' property missing") (M.lookup lbl jObj)
            sp <- gDecode' (val unit) pf
            pure { recLabel: lbl, recValue: const sp }

    SigProd "Data.Maybe.Maybe" alts -> do
      justDC <- case head alts of
        Nothing -> unsafeThrow "Impossible unless Maybe has been redefined."
        Just x -> Right x

      sps  <- zipWithA (\k -> gDecode' (k unit)) justDC.sigValues [json]
      sp <- case head sps of
        Nothing -> unsafeThrow "Impossible"
        Just x -> Right x

      pure $ SProd justDC.sigConstructor [\_ -> sp]

    SigProd typeConstr alts -> do
      let decodingErr msg = "When decoding a " ++ typeConstr ++ ": " ++ msg

      _ <- if length alts == 1
           then Right unit
           else Left (decodingErr ("Must have exactly one data constructor."))

      tp <- case head alts of
        Nothing -> unsafeThrow "Impossible we have exactly one data constructor."
        Just x -> Right x

      sps  <- zipWithA (\k -> gDecode' (k unit)) tp.sigValues [json]
      sp <- case head sps of
        Nothing -> unsafeThrow "Impossible"
        Just x -> Right x

      pure $ SProd tp.sigConstructor [\_ -> sp]
  where
    mFail :: forall a. String -> Maybe a -> Either String a
    mFail msg = maybe (Left msg) Right



gDecode :: forall a. (Generic a) => Json -> Either String a
gDecode json = maybe (Left "fromSpine failed") Right <<< fromSpine
               =<< gDecode' (toSignature (Proxy :: Proxy a)) json
