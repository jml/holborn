{-| The following example program shows how to use 'execute' to spawn the
    shell command @(grep -v foo)@.  This program forwards
    @Pipes.ByteString.'Pipes.ByteString.stdin'@ to the standard input of @grep@
    and merges the standard output and standard error of @grep@ to
    @Pipes.ByteString.'Pipes.ByteString.stdout'@:

> import Control.Applicative ((<|>))
> import Pipes (runEffect, (>->))
> import Pipes.ByteString (stdin, stdout)
> import Pipes.Process
>
> main = execute (proc "grep" ["-v", "foo"]) $ \grepIn grepOut grepErr -> do
>     forkIO $ runEffect $ stdin >-> toOutput grepIn
>     forkIO $ runEffect $ fromInput (grepOut <|> grepErr) >-> stdout
-}

module Holborn.Repo.GabrielProcess (
    -- * Types
      Stdin
    , Stdout
    , Stderr

    -- * Spawn processes
    , execute
    , execute'

    -- * Re-exports
    -- $reexports
    , module Data.ByteString
    , module Pipes.Concurrent
    , module System.Exit
    , module System.Process
    ) where

import Prelude

import Data.Foldable (forM_)
import Data.ByteString (ByteString)
import Pipes (runEffect, (>->))
import Pipes.ByteString (fromHandle, toHandle)
import Pipes.Concurrent
import System.Exit (ExitCode)
import System.Process
import System.IO (BufferMode(..), hSetBuffering)
import Control.Concurrent (forkIO)

-- | Use 'send' or 'toOutput' to write to 'Stdin'
type Stdin = Output ByteString

-- | Use 'recv' or 'fromInput' to read from 'Stdout'
type Stdout = Input ByteString

-- | Use 'recv' or 'fromInput' to read from 'Stdin'
type Stderr = Input ByteString

{-| Spawn a process as specified by 'CreateProcess', returning:

    * An 'Output' that writes to standard input for the process

    * An 'Input' that reads from standard output for the process

    * An 'Input' that reads from standard error for the process

    'execute' buffers these three interfaces using an 'Unbounded' buffer.  Use
    'execute'' to specify alternative 'Buffer's.

    'execute' terminates when the process terminates.
-}
execute
    :: CreateProcess
    -- ^ Process instructions
    -> (Stdin -> Stdout -> Stderr -> IO r)
    -- ^ Callback
    -> IO (ExitCode, r)
execute c k = execute' Unbounded Unbounded Unbounded c $ \oIn _ iOut _ iErr _ ->
    k oIn iOut iErr
{-# INLINABLE execute #-}

{-| This is a more general variation on 'execute' that also lets you:

    * specify how to 'Buffer' 'Stdin' \/ 'Stdout' \/ 'Stderr', and:

    * seal each 'Buffer' manually.
-}
execute'
    :: Buffer ByteString
    -- ^ Buffer for standard input
    -> Buffer ByteString
    -- ^ Buffer for standard output
    -> Buffer ByteString
    -- ^ Buffer for standard error
    -> CreateProcess
    -- ^ Process instructions
    -> (Stdin -> STM () -> Stdout -> STM () -> Stderr -> STM () -> IO r)
    -- ^ Callback with seal actions for each buffer
    -> IO (ExitCode, r)
execute' bufIn bufOut bufErr c k = do
    (oIn , iIn , sIn ) <- spawn' bufIn
    (oOut, iOut, sOut) <- spawn' bufOut
    (oErr, iErr, sErr) <- spawn' bufErr
    (mIn, mOut, mErr, ph) <- createProcess c

    forkIO $ forM_ mIn  $ \hIn  -> runEffect $ fromInput  iIn  >-> toHandle hIn
    forkIO $ forM_ mOut $ \hOut -> runEffect $ fromHandle hOut >-> toOutput oOut
    forkIO $ forM_ mErr $ \hErr -> runEffect $ fromHandle hErr >-> toOutput oErr

    r  <- k oIn sIn iOut sOut iErr sErr
    atomically $ do
        sIn
        sOut
        sErr
    ec <- waitForProcess ph
    return (ec, r)
{-# INLINABLE execute' #-}

{- $reexports
    "Data.ByteString" re-exports 'ByteString'

    "Pipes.Concurrent" re-exports everything

    "System.Exit" re-exports 'ExitCode'

    "System.Process" re-exports everything
-}
