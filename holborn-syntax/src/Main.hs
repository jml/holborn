{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicPrelude

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
  ( serve
  , Get
  , Proxy(..)
  , Server
  )
import Servant.HTML.Blaze

-- Get the typeclass instances for converting Holborn stuff to HTML.
import Holborn.HtmlFormat ()
import Holborn.Scope (ID)
import Holborn.Syntax (annotatePythonCode)
import Holborn.Types (AnnotatedSource)


-- Simplistic demo that renders a single Python file with links from its
-- references to its bindings.

-- TODO: Read the Python file from disk
-- TODO: Render multiple Python files
-- TODO: Have this be a purely JSON API and have client-side code do the rendering
-- TODO: Specify the path to Python files via an environment variable
-- TODO: Specify port as an environment variable

import ExampleData (examplePython)


type RootAPI = Get '[HTML] (AnnotatedSource ID)


rootAPI :: Proxy RootAPI
rootAPI = Proxy

server :: Server RootAPI
server = return (annotatePythonCode examplePython)

app :: Application
app = serve rootAPI server

main :: IO ()
main = run 8080 app
