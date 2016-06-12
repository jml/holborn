{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import HolbornPrelude

import Turtle
import Control.Concurrent (threadDelay)
import System.Exit (exitWith)
import Network.Simple.TCP (connectSock)

parser :: Parser (Int, Int)
parser = (,) <$> argInt "port" "port to check"
             <*> optInt "timeout" 't' "timeout in seconds before failing"

main :: IO ()
main = do
    (port, timeout) <- options "Wait for a port to become active for --timeout seconds, then fail if it didn't. Retries once a second." parser
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
