-- | Prelude for all Holborn code.
--
-- Protolude + some extensions.

{-# LANGUAGE FlexibleContexts #-}

module HolbornPrelude
  ( -- * Module exports
    module Protolude

    -- * Backwards compatibility
    --
    -- To make migrating to Protolude easier.
  , id
  , terror
    -- | Protolude doesn't export much String stuff, this is mostly a good
    -- thing, but we're going to export some String stuff to make our lives
    -- easier.
  , IsString(..)
  , String
  , fromShow
  , textToString
    -- | Options.Applicative 0.12.1 implements monoid, not semigroup.
    -- Protolude exports (<>) from semigroup. We can remove this after
    -- upgrading to optparse-applicative 0.13 or later.
  , (<>)
    -- | Protolude's versions of these are generic, which requires explict
    -- types at many call sites. Protolude *does* provide 'putText' as a
    -- Text-specialized alias 'putStrLn' but for now, export Text-specific
    -- versions.
  , putStr
  , putStrLn

    -- * Holborn extensions and innovations
    -- ** Useful types
  , Hashable
    -- ** Generic versions of list functions.
    --
    -- In standard Prelude, these are specialised to lists. In BasicPrelude,
    -- they are more generic. In Protolude, they are back to being
    -- specialised. Arguably we should use the generic versions everywhere and
    -- not have these here.
  , (++)  -- Use (<>) instead
  , concat  -- Use fold instead
  , intercalate
    -- ** Error handling
  , hush
  , fmapL
  , note

    -- ** Debugging
  , printErr
  ) where

import Data.Hashable (Hashable)
import qualified Data.List
import Data.Monoid ((<>))
import GHC.Base (String)
import GHC.Exts (IsString(..))
import System.IO (hPutStrLn)

import Protolude hiding ((<>), (++), concat, intercalate, putStr, putStrLn, fromStrict, yield)
import qualified Base
import qualified Show

(++) :: Monoid m => m -> m -> m
(++) = (<>)
infixr 5 ++

concat :: (Foldable t, Monoid a) => t a -> a
concat = fold

{-# DEPRECATED id "Use 'identity' instead" #-}
id :: a -> a
id = identity

{-# DEPRECATED fromShow "Use 'show' instead" #-}
fromShow :: (Show a, StringConv String b) => a -> b
fromShow = show

putStr :: MonadIO m => Text -> m ()
putStr = Show.putStr

putStrLn :: MonadIO m => Text -> m ()
putStrLn = Show.putStrLn

-- | Print to standard error.
-- TODO: Replace 'Text' with generic string type.
printErr :: Text -> IO ()
printErr = hPutStrLn stderr . toS

-- | Generalized intercalate (from basic-prelude).
--
--   > intercalate = mconcat .: intersperse
intercalate :: Monoid w => w -> [w] -> w
intercalate xs xss = mconcat (Data.List.intersperse xs xss)

{-# DEPRECATED terror "Use 'error' instead" #-}
terror :: Text -> a
terror = Base.error . toS

{-# DEPRECATED textToString "Use 'toS' instead" #-}
textToString :: Text -> String
textToString = toS

-- | Generic version of 'hush' from Control.Errors
hush :: Alternative m => Either e a -> m a
hush (Left _)  = empty
hush (Right x) = pure x

-- | Generic version of 'note' from Control.Errors
note :: (MonadError e m) => e -> Maybe a -> m a
note err = maybe (throwError err) pure

-- | Map over the Left value of an Either
--
-- > fmapL (+1) (Left 42)
-- Left 43
--
-- > fmapL (+1) (Right 11)
-- Right 11
fmapL :: (a -> b) -> Either a c -> Either b c
fmapL f (Left x)  = Left (f x)
fmapL _ (Right x) = Right x
