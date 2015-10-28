module Main where

import BasicPrelude

import Turtle
import Control.Concurrent (threadDelay)
import System.Exit (exitWith)

parser :: Parser (Int, Int)
parser = (,) <$> argInt "port" "port to check"
             <*> optInt "timeout" 't' "timeout in seconds before failing"

main :: IO ()
main = do
    (port, timeout) <- options "Wait for a port to become active for --timeout seconds, then fail if it didn't. Retries once a second." parser
    (check port timeout) >>= exitWith

  where
    check :: Int -> Int -> IO ExitCode
    check port timeout
      | timeout <= 0 = return (ExitFailure 1)
      | otherwise = do
        exitCode <- proc "nc" ["-z", "127.0.0.1", show port] Turtle.empty
        case exitCode of
            ExitSuccess -> return ExitSuccess
            ExitFailure _ -> do
                threadDelay (1000 * 1000)
                check port (timeout - 1)
