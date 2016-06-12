-- | This module runs the correct cabal invocation for figuring out
-- modules that are specified in cabal but not actually imported
-- anywhere. Useful for the occasional cleanup.

module Main (main) where

import HolbornPrelude hiding (empty)
import Turtle (shells, empty)

main :: IO ()
main = do
    shells "cabal clean" empty
    shells "cabal configure -O0 --disable-library-profiling" empty
    shells "cabal build --ghc-option=-ddump-minimal-imports" empty
    shells "packunused" empty
