module Holborn.Repo.Process
       ( streamIO
       , Process.proc
       ) where

import           HolbornPrelude hiding (stdin, stdout)

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar (newMVar, modifyMVar_)
import           Pipes ((>->), runEffect)
import           Pipes.ByteString (fromHandle, toHandle)
import           Pipes.Core (Consumer, Producer)
import           System.IO (hClose, hSetBuffering, BufferMode(..))
import qualified System.Process as Process
import           System.Exit (ExitCode(..))

-- | Bidirectional process IO
--
-- TODO:
-- * ship stderr to the "correct" log
-- * test this a bit harder
streamIO :: Process.CreateProcess -> Producer ByteString IO () -> Consumer ByteString IO () -> IO ExitCode
streamIO p stdin stdout = do
    -- Prevent double close (borrowed from Gabriel's Turtle code)
    mvar <- newMVar False
    bracket
        open
        (\(hIn, _, ph) -> close mvar hIn *> Process.terminateProcess ph)
        run
  where
    p' = p { Process.std_in  = Process.CreatePipe
           , Process.std_out = Process.CreatePipe
           , Process.std_err = Process.Inherit
           }
    open = do
        (Just hIn, Just hOut, Nothing, ph) <- liftIO (Process.createProcess p')
        hSetBuffering hIn NoBuffering
        hSetBuffering hOut NoBuffering
        return (hIn, hOut, ph)

    close mvar h = do
        modifyMVar_ mvar (\finalized -> do
                               unless finalized (hClose h)
                               return True)

    -- | TODO I don't fully understand the resource cleanup yet. I
    -- think the async library does this better than forkIO but I'm
    -- out of energy on process communication.
    run (hIn, hOut, ph) = do
        void $ forkIO (runEffect (stdin >-> toHandle hIn))
        void $ runEffect (fromHandle hOut >-> stdout)
        Process.waitForProcess ph
