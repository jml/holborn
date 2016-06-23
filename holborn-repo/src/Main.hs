{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import HolbornPrelude

import Control.Concurrent (forkIO)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (runSettings)
import qualified Network.Wai.Middleware.RequestLogger as RL
import Servant (serve)
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

import Holborn.Repo (repoServer, repoAPI)
import Holborn.Repo.Config (Config(..), warpSettings)
import Holborn.Repo.RawProtocol (serveRaw)


options :: ParserInfo Config
options =
  info (helper <*> parser) description
  where
    parser =
      Config
      <$> option str
          ( long "repo-root"
            <> metavar "REPO_ROOT"
            <> help "Path to root of ./<repoId> bare repositories" )
      <*> option auto
          ( long "http-port"
            <> metavar "HTTP_PORT"
            <> help "Port to listen on for HTTP API"
            <> value 8080 )
      <*> option auto
          ( long "git-port"
            <> metavar "GIT_PORT"
            <> help "Port to listen on for git-serve requests"
            <> value 8081 )

    description = concat
      [ fullDesc
      , progDesc "Serve git repositories under REPO_ROOT"
      , header "holborn-repo - serve git repositories for pushing, pulling, and browsing"
      ]


-- git init --bare /tmp/hello
app :: Config -> Application
app config = serve repoAPI (repoServer config)


main :: IO ()
main = do
  config <- execParser options
  void $ forkIO (serveRaw config)
  runSettings (warpSettings config) (RL.logStdoutDev (app config))
