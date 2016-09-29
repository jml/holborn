{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import HolbornPrelude

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
  , switch
  , str
  , value
  )
import System.Exit (exitWith)


data Config = Config { port :: Int
                     , timeout :: Int
                     , hostname :: String
                     , verbose :: Bool
                     }


options :: ParserInfo Config
options = info (helper <*> parser) description
  where
    parser = Config <$> option auto ( long "port" <> metavar "PORT" <> help "port to check" )
                    <*> option auto ( long "timeout" <> short 't' <> metavar "TIMEOUT"
                                      <> help "seconcds to wait before failing" )
                    <*> option str ( long "host" <> metavar "HOST" <> help "host to check"
                                     <> value "127.0.0.1" )
                    <*> switch ( long "verbose" <> short 'v' <> help "Print out each connection attempt" )
    description = concat
      [ fullDesc
      , progDesc "Wait for --timeout seconds for a port to become active, then fail if it didn't. Retries in 50ms then backs off exponentially."
      , header "wait-for-port - wait for a port to become active"
      ]

serverAddr :: String -> Int -> Text
serverAddr host port = fromString host <> ":" <> show port

-- | Repeatedly poll host:port until we connect or we reach a timeout.
--
-- Delay and timeout are both in milliseconds.
waitForPort :: String -> Int -> Int -> Int -> Bool -> IO ExitCode
waitForPort host port timeout delay verbose
  | timeout <= 0 = do
      printErr $ "Could not connect to " <> serverAddr host port
      pure (ExitFailure 1)
  | otherwise = do
      when verbose $ putStr $ "Connecting to " <> serverAddr host port <> " ... "
      result <- try (connectSock host (show port))
      case result of
        Left (_ :: IOException) -> do
          when verbose $ putStr "Failed\n"
          threadDelay (delay * 1000)
          waitForPort host port (timeout - delay) (delay * 2) verbose
        Right _ -> do
          when verbose $ putStr "OK\n"
          return ExitSuccess


main :: IO ()
main = do
    Config{port, timeout, hostname, verbose} <- execParser options
    exitCode <- waitForPort hostname port (timeout * 1000) 50 verbose
    exitWith exitCode
