We can instrument code with our own trace points:

http://www.well-typed.com/blog/86/

Note that the "START xx" and "STOP xx" events are defaults in the ghc-events-analyze package:

https://github.com/well-typed/ghc-events-analyze/blob/11ca33fc2fca3c954df3ec719e2a0075da04d502/src/GHC/RTS/Events/Analyze/Options.hs#L52

log user events:

stdout:
cabal run -- +RTS -vtu

eventlog:
cabal run -- +RTS -lu
