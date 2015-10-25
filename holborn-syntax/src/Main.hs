{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicPrelude

import qualified Env
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (serve)
import System.Directory (canonicalizePath)

import Holborn.Web (syntaxAPI, server)

import ExampleData (examplePython)


-- XXX: I can imagine a more elaborate version of this, where we have one
-- thing that parses Warp-related settings out of the environment and returns
-- a Warp.Settings, and another thing that handles application- or
-- library-specific settings. This would then allow a standard /configz style
-- page, which would be super-useful for debugging. However, keep it simple
-- for now: just pass relevant stuff as arguments to functions.

data Config = Config { _port :: Warp.Port
                     , _codePath :: FilePath
                     }

loadConfig :: IO Config
loadConfig =
  Env.parse (Env.header "run a holborn syntax server") $
  Config <$> Env.var Env.auto "PORT"       (Env.def 8080 <> Env.help "Port to listen on")
         -- XXX: Better name than "FILES_PATH"
         <*> Env.var Env.str  "FILES_PATH" (Env.def "."  <> Env.help "Location of source files to browse")


app :: Text -> FilePath -> Application
app demoCode basePath = serve syntaxAPI (server demoCode basePath)


warpSettings :: Warp.Port -> Warp.Settings
warpSettings port =
  (Warp.setPort port Warp.defaultSettings)


main :: IO ()
main = do
  config <- loadConfig
  codePath <- canonicalizePath $ _codePath config
  Warp.runSettings (warpSettings (_port config)) (app examplePython codePath)
