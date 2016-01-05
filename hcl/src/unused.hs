module Main where

import Turtle

main = do
    shell "cabal clean" empty
    shell "cabal configure -O0 --disable-library-profiling" empty
    shell "cabal build --ghc-option=-ddump-minimal-imports" empty
    shell "packunused" empty
