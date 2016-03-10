-- | This module runs the correct cabal invocation for figuring out
-- modules that are specified in cabal but not actually imported
-- anywhere. Useful for the occasional cleanup.

module Main (main) where

import Turtle (shells, empty)

main = do
    shells "cabal clean" empty
    shells "cabal configure -O0 --disable-library-profiling" empty
    shells "cabal build --ghc-option=-ddump-minimal-imports" empty
    shells "packunused" empty
