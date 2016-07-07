-- | Prelude for all Holborn code.
--
-- Heavily inspired by BasicPrelude.

module HolbornPrelude
  ( -- * Module exports
    module CorePrelude
  , module Data.List
  , module Control.Applicative
  , module Control.Monad

    -- ** Folds and traversals
  , Foldable
    (
      foldMap
    , foldr
    , foldr'
    , foldl
    , foldl'
    , foldr1
    , foldl1
    )
    -- In base-4.8, these are instance methods.
  , elem
  , maximum
  , minimum
  , Traversable
    (
      traverse
    , sequenceA
    , mapM
    , sequence
    )

    -- * Enhanced exports
    -- ** Simpler name for a typeclassed operation
  , map
  , (++)
  , concat
  , intercalate
    -- ** Strict implementation
  , HolbornPrelude.sum
  , HolbornPrelude.product
    -- ** Text for Read and Show operations
  , show
  , fromShow
  , read
  , readIO
    -- ** FilePath for file operations
  , readFile
  , writeFile
  , appendFile

    -- * Text exports
    -- ** Text operations (Pure)
  , Text.lines
  , Text.words
  , Text.unlines
  , Text.unwords
  , textToString
  , ltextToString
  , encodeUtf8
  , decodeUtf8
    -- ** Text operations (IO)
  , Text.getLine
  , LText.getContents
  , LText.interact

    -- * Miscellaneous prelude re-exports
    -- ** Math
  , Prelude.gcd
  , Prelude.lcm
    -- ** Show and Read
  , Prelude.ShowS
  , Prelude.showsPrec
  , Prelude.showList
  , Prelude.shows
  , Prelude.showChar
  , Prelude.showString
  , Prelude.showParen
  , Prelude.ReadS
  , Prelude.readsPrec
  , Prelude.readList
  , Prelude.reads
  , Prelude.readParen
  , Prelude.lex
  , readMay
    -- ** IO operations
  , Prelude.putChar
  , Prelude.getChar
  , Prelude.readLn

    -- * Holborn extensions and innovations
    -- ** Error handling
  , hush
  , fmapL
  , note
  ) where

import CorePrelude

import Data.List hiding
  ( -- prefer monoid versions instead
    (++)
  , concat
  , intercalate
    -- prefer Text versions instead
  , lines
  , words
  , unlines
  , unwords
    -- prefer map = fmap instead
  , map
    -- prefer strict versions
  , sum
  , product
    -- prefer Foldable versions
  , elem
  , foldl
  , foldl'
  , foldl1
  , foldr
  , foldr1
  , maximum
  , minimum
  )

-- Import *all of the things* from Control.Monad,
-- specifically, the list-based things that
-- CorePrelude doesn't export
import Control.Monad hiding
  ( -- Also exported by Data.Traversable.
    mapM
  , sequence
  )


import Control.Applicative
import Control.Monad.Except (MonadError, throwError)
import Data.Foldable (Foldable(..), elem, maximum, minimum)
import Data.Traversable (Traversable(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import qualified Prelude
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Safe

-- | > map = fmap
map :: (Functor f) => (a -> b) -> f a -> f b
map = fmap

infixr 5 ++

-- | > (++) = mappend
(++) :: Monoid w => w -> w -> w
(++) = mappend

-- | > concat = mconcat
concat :: Monoid w => [w] -> w
concat = mconcat

-- | > intercalate = mconcat .: intersperse
intercalate :: Monoid w => w -> [w] -> w
intercalate xs xss = mconcat (Data.List.intersperse xs xss)


-- | Compute the sum of a finite list of numbers.
sum :: Num a => [a] -> a
sum = Data.Foldable.foldl' (+) 0

-- | Compute the product of a finite list of numbers.
product :: Num a => [a] -> a
product = Data.Foldable.foldl' (*) 1


-- | Convert a value to readable Text
show :: Show a => a -> Text
show = Text.pack . Prelude.show

-- | Convert a value to readable IsString
--
-- Since 0.3.12
fromShow :: (Show a, IsString b) => a -> b
fromShow = fromString . Prelude.show

-- | Parse Text to a value
read :: Read a => Text -> a
read = Prelude.read . Text.unpack

-- | The readIO function is similar to read
-- except that it signals parse failure to the IO monad
-- instead of terminating the program.
readIO :: Read a => Text -> IO a
readIO = Prelude.readIO . Text.unpack


-- | Read a file and return the contents of the file as Text.
-- The entire file is read strictly.
readFile :: FilePath -> IO Text
readFile = Text.readFile

-- | Write Text to a file.
-- The file is truncated to zero length before writing begins.
writeFile :: FilePath -> Text -> IO ()
writeFile = Text.writeFile

-- | Write Text to the end of a file.
appendFile :: FilePath -> Text -> IO ()
appendFile = Text.appendFile

textToString :: Text -> Prelude.String
textToString = Text.unpack

ltextToString :: LText -> Prelude.String
ltextToString = LText.unpack

-- | Note that this is /not/ the standard @Data.Text.Encoding.decodeUtf8@. That
-- function will throw impure exceptions on any decoding errors. This function
-- instead uses @decodeLenient@.
decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode

readMay :: Read a => Text -> Maybe a
readMay = Safe.readMay . Text.unpack


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
