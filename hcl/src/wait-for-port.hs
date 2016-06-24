{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import HolbornPrelude

import Turtle hiding (option, options)
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
  )
import System.Exit (exitWith)


options :: ParserInfo (Int, Int)
options = info (helper <*> parser) description
  where
    parser = (,) <$> option auto ( long "port" <> metavar "PORT" <> help "port to check" )
                 <*> option auto ( long "timeout" <> short 't' <> metavar "TIMEOUT"
                                   <> help "seconcds to wait before failing" )
    description = concat
      [ fullDesc
      , progDesc "Wait for --timeout seconds for a port to become active, then fail if it didn't. Retries once a second."
      , header "wait-for-port - wait for a port to become active"
      ]

main :: IO ()
main = do
    (port, timeout) <- execParser options
    check port timeout >>= exitWith

  where
    check :: Int -> Int -> IO ExitCode
    check port timeout
      | timeout <= 0 = do
            print ("Could not connect to " :: String, port)
            return (ExitFailure 1)
      | otherwise = do
            result <- try (connectSock "127.0.0.1" ((textToString . show) port))
            case result of
             Left (_ :: IOException) -> do
                 threadDelay (1000 * 1000)
                 check port (timeout - 1)
             Right _ -> return ExitSuccess
