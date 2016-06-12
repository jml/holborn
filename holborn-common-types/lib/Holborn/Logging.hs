-- | Simple logging library
--
-- Tries to provide the bare minimum until we figure out how we really want to
-- do logging.

module Holborn.Logging (debug, info) where

import HolbornPrelude
import System.IO (stdout, hFlush)


-- | Log a debugging message.
debug :: (MonadIO m, Show s) => s -> m ()
debug message = do
  print message
  liftIO $ hFlush stdout


-- | Log an informational message.
info :: (MonadIO m, Show s) => s -> m ()
info = debug
