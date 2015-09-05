module Main where

import BasicPrelude
import Web.Spock.Safe

import Holborn.HtmlFormat ()
import Holborn.Web (codePage, annotatePythonCode)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html (Html)


import ExampleData (examplePython)

-- XXX: Is there a better way of sending blaze to the user with Spock?
blaze :: MonadIO m => Html -> ActionT m a
blaze = lazyBytes . renderHtml

main :: IO ()
main =
    runSpock 8080 $ spockT id $
    get root $ blaze $ codePage $ annotatePythonCode examplePython
