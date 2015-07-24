module Main where

import BasicPrelude

import Web.Spock.Safe

import ExampleData (examplePython)


main :: IO ()
main =
    runSpock 8080 $ spockT id $
    do get root $
           text examplePython
       get ("hello" <//> var) $ \name ->
           text ("Hello " <> name <> "!")
