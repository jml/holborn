{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicPrelude

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (serve)

import Holborn.Web (syntaxAPI, server)


-- Simplistic demo that renders a single Python file with links from its
-- references to its bindings.

-- TODO: Render multiple Python files
-- TODO: Have this be a purely JSON API and have client-side code do the rendering
-- TODO: Specify the path to Python files via an environment variable
-- TODO: Specify port as an environment variable

import ExampleData (examplePython)


app :: Text -> FilePath -> Application
app demoCode basePath = serve syntaxAPI (server demoCode basePath)

main :: IO ()
main = run 8080 (app examplePython ".")
