{-# LANGUAGE RecursiveDo #-}
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
    (port, timeOut) <- options "Wait for a port to become active for --timeout seconds, then fail if it didn't" parser
    (check port timeOut) >>= exitWith

  where
    check :: Int -> Int -> IO ExitCode
    check _ 0 =
        return (ExitFailure 1)
    check port timeOut = do
        exitCode <- proc "nc" ["-z", "127.0.0.1", show port] Turtle.empty
        case exitCode of
            ExitSuccess -> return ExitSuccess
            ExitFailure _ -> do
                threadDelay (1000 * 1000)
                check port (timeOut - 1)
