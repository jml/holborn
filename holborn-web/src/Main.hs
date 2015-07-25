module Main where

import BasicPrelude
import Web.Spock.Safe

import Holborn.Web (codePage, renderPythonCode)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html (Html)


import ExampleData (examplePython, exampleAnnotation)

-- XXX: Is there a better way of sending blaze to the user with Spock?
blaze :: MonadIO m => Html -> ActionT m a
blaze = lazyBytes . renderHtml

main :: IO ()
main =
    runSpock 8080 $ spockT id $
    get root $ blaze $ codePage $ renderPythonCode examplePython exampleAnnotation
