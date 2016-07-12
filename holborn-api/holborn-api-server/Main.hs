{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import HolbornPrelude

import Database.PostgreSQL.Simple (ConnectInfo(..), defaultConnectInfo)
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (cors, CorsResourcePolicy(..), simpleCorsResourcePolicy, simpleHeaders)
import qualified Network.Wai.Middleware.RequestLogger as RL
import Options.Applicative
  ( ParserInfo
  , auto
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , progDesc
  , str
  , value
  )
import Servant (serve)

import Holborn.API (api, server)
import Holborn.API.Config (Config(..))
import qualified Holborn.Logging as Log


options :: ParserInfo Config
options =
  info (helper <*> parser) description
  where
    parser =
      Config
      <$> option auto
          ( long "port" <> metavar "PORT" <> help "Port to listen on" )
      <*> (postgres
           <$> option auto  -- TODO: Where's the hostname?
               ( long "postgres-port" <> metavar "PORT" <> value 5432
                 <> help "Port the PostgreSQL database is running on" )
           <*> option str
               ( long "postgres-user" <> metavar "USER" <> value "holborn"
                 <> help "Username for the PostgreSQL database" )
           <*> option str  -- TODO: reject empty names
               ( long "postgres-database" <> metavar "DATABASE" <> value "holborn"
                 <> help "Name of PostreSQL database with the holborn data" ))
      <*> option (fromString <$> str)  -- TODO: What happens if the port contradicts the --port flag? Also, how do we use this?
          ( long "base-url" <> metavar "URL" <> value "http://127.0.0.1:8002"
            <> help "URL for the REST API server" )
      <*> option (fromString <$> str)
          ( long "static-url" <> metavar "URL" <> value "http://127.0.0.1:1337"
            <> help "URL for the static content of the holborn app" )
      <*> option (fromString <$> str)
          ( long "repo-hostname" <> metavar "HOST" <> value "127.0.0.1"
            <> help "Where the holborn-repo server is running" )
      <*> option auto
          ( long "repo-http-port" <> metavar "PORT" <> value 8080
            <> help "HTTP API port for the holborn-repo server" )
      <*> option auto
          ( long "repo-git-port" <> metavar "PORT" <> value 8081
            <> help "git-serve port for the holborn-repo server" )

    postgres port user database =
      defaultConnectInfo { connectPort = port
                         , connectUser = user
                         , connectDatabase = database
                         }

    description = concat
      [ fullDesc
      , progDesc "Launch API server for holborn"
      , header "holborn-api - REST interface to the entire holborn project"
      ]

-- XXX: Duplicated & modified from Holborn.Repo.Config
-- | Generate warp settings from config
--
-- Serve from a port and print out where we're serving from.
warpSettings :: Warp.Port -> Warp.Settings
warpSettings port =
  Warp.setBeforeMainLoop printPort (Warp.setPort port' Warp.defaultSettings)
  where
    printPort = Log.info $ "holborn-api running at http://localhost:" ++ show port' ++ "/"
    port' = port


devCors :: a -> Maybe CorsResourcePolicy
devCors _ = Just (simpleCorsResourcePolicy { corsRequestHeaders = simpleHeaders ++ ["Authorization"] })


app :: Config -> Application
app conf = serve api (server conf)


main :: IO ()
main = do
  conf@Config{port} <- execParser options
  putStrLn $ "Using config: " <> show conf
  Warp.runSettings (warpSettings port) (RL.logStdoutDev (cors devCors (app conf)))
