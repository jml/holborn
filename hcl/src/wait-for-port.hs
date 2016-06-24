{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import HolbornPrelude

import Turtle hiding (hostname, option, options)
import Control.Concurrent (threadDelay)
import Network.Simple.TCP (connectSock)
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
  , short
  , str
  , value
  )
import System.Exit (exitWith)


data Config = Config { port :: Int
                     , timeout :: Int
                     , hostname :: String }


options :: ParserInfo Config
options = info (helper <*> parser) description
  where
    parser = Config <$> option auto ( long "port" <> metavar "PORT" <> help "port to check" )
                    <*> option auto ( long "timeout" <> short 't' <> metavar "TIMEOUT"
                                      <> help "seconcds to wait before failing" )
                    <*> option str ( long "host" <> metavar "HOST" <> help "host to check"
                                     <> value "127.0.0.1" )
    description = concat
      [ fullDesc
      , progDesc "Wait for --timeout seconds for a port to become active, then fail if it didn't. Retries in 50ms then backs off exponentially."
      , header "wait-for-port - wait for a port to become active"
      ]

-- | Repeatedly poll host:port until we connect or we reach a timeout.
--
-- Delay and timeout are both in milliseconds.
waitForPort :: String -> Int -> Int -> Int -> IO ExitCode
waitForPort host port timeout delay
  | timeout <= 0 = do
      print ("Could not connect to " <> fromString host <> ":" <> show port)
      pure (ExitFailure 1)
  | otherwise = do
      result <- try (connectSock host ((textToString . show) port))
      case result of
        Left (_ :: IOException) -> do
          threadDelay (delay * 1000)
          waitForPort host port (timeout - delay) (delay * 2)
        Right _ -> return ExitSuccess


main :: IO ()
main = do
    Config{port, timeout, hostname} <- execParser options
    exitCode <- waitForPort hostname port (timeout * 1000) 50
    exitWith exitCode
