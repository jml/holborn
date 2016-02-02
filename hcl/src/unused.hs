-- | This module runs the correct cabal invocation for figuring out
-- modules that are specified in cabal but not actually imported
-- anywhere. Useful for the occasional cleanup.

module Main (main) where

import Turtle

main = do
    shell "cabal clean" empty
    shell "cabal configure -O0 --disable-library-profiling" empty
    shell "cabal build --ghc-option=-ddump-minimal-imports" empty
    shell "packunused" empty
