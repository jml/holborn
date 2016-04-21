-- | This module sets up a standardized postgres database and then
-- runs the command given with -c with the PG database being alive +
-- correct environment variables exported
--
-- Cleans up after shutdown.
--
-- If some environment variable is weird you can check
-- by running export like so:
-- cabal run hcl-dev-db --  -c "export" -s ../holborn-api/sql/initial.sql


module Main (main) where

import BasicPrelude (($), liftIO, print, Text, pure, (<*>), (<$>), show, forM, Maybe(..), (>>))

import Turtle (shells, empty, mktempdir, procs, using, sh, (</>),
               format, fp, echo, (%), s, fork, wait, export, optText, d,
               options, Parser, FilePath, optPath)
import Control.Concurrent (forkIO, threadDelay, myThreadId, killThread)
import Control.Concurrent.Async (cancel)
import Data.Text (splitOn)
import System.Posix.Signals (Handler(Catch), installHandler, sigTERM)


pgPort = 5444

parser :: Parser (Text, Text)
parser = (,)
  <$> optText "command" 'c' "Command to run in database context"
  <*> optText "sql_init" 's' "Path to sql init stanza, can be comma separated list of files"

main = sh $ do
    (command, sql_paths) <- options "hcl-dev-db" parser
    let sql_path = splitOn "," sql_paths
    myTid <- liftIO myThreadId

    tempPath <- using (mktempdir "/tmp" "postgres-data")
    let dbPath = format fp (tempPath </> "db")
    echo (format ("Running DB in " % s) dbPath)
    procs "initdb" ["-D", dbPath] empty

    pgProcess <- using (fork $ procs "postgres" ["-D", dbPath, "-p", show pgPort] empty)
    liftIO (threadDelay 200000) -- TODO use wait for port code
    let killPg = procs "pkill" ["-TERM", "-f", dbPath] empty
    liftIO (installHandler sigTERM (Catch (sh killPg >> killThread myTid))  Nothing)

    procs "createuser" ["-p", show pgPort, "holborn-test"] empty
    procs "createdb" ["-p", show pgPort, "holborn-test", "-O", "holborn-test"] empty
    forM sql_path (\path -> procs "psql" ["-p", show pgPort, "holborn-test", "-U", "holborn-test", "-f", path] empty)

    export "HOLBORN_PG_DATABASE" "holborn-test"
    export "HOLBORN_PG_USER" "holborn-test"
    export "HOLBORN_PG_PORT" (show pgPort)

    shells command empty

    killPg
