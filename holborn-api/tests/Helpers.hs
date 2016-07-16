-- | Support code for holborn-api integration tests.

module Helpers
  ( Postgres(..)
  , connection
  , makeDatabase
  , stopPostgres
  ) where

import HolbornPrelude

import Data.Word (Word16)
import Database.PostgreSQL.Simple (ConnectInfo(..), defaultConnectInfo)
import System.Process
  ( callCommand
  , showCommandForUser
  )
import System.Posix.Temp (mkdtemp)

-- | A TCP port.
type Port = Word16

-- | Postgres username.
type UserName = String

-- | Postgres database name.
type DatabaseName = String

data Postgres =
  Running { port      :: Port
          , username  :: UserName
          , database  :: DatabaseName
          , directory :: FilePath
          }


connection :: Postgres -> ConnectInfo
connection Running{port, username, database} =
  defaultConnectInfo { connectPort = port
                     , connectUser = username
                     , connectDatabase = database
                     }


-- | Make sure we have a database made of the SQL that's in the given file.
makeDatabase :: FilePath -> IO Postgres
makeDatabase schema = do
  -- TODO: Try to re-use existing one, somehow
  pgDir <- initDb
  -- TODO: Try to re-use this?
  -- TODO: bracket the launch w/ a safe teardOwn
  port <- launchPostgres pgDir
  user <- createPostgresUser port
  database <- createPostgresDatabase port user
  let postgres = Running { port      = port
                         , username  = user
                         , database  = database
                         , directory = pgDir
                         }
  runSqlFromFile postgres schema
  pure postgres

  where
    -- | Initialize a Postgresql directory.
    initDb :: IO FilePath
    initDb = do
      dir <- mkdtemp "/tmp/holborn-postgres"
      -- XXX: It'd be nice not to have to a) assemble the string here b) go
      -- via shell.
      cmd "pg_ctl" ["init", "-D", dir, "-s"]
      pure dir

    -- | Start a Postgresql instance using the given directory.
    --
    -- Blocks until the Postgresql instance is ready for use.
    launchPostgres :: FilePath -> IO Port
    launchPostgres pgDir = do
      let portOpt = "-p " <> textToString (show port)
      cmd "pg_ctl" ["start", "-D", pgDir, "-s", "-w", "-o", portOpt]
      pure port
        where
          -- TODO: Don't hardcode this.
          port = 5444

    -- | Create a valid postgres user and return their name.
    createPostgresUser :: Port -> IO UserName
    createPostgresUser port = do
      cmd "createuser" ["-p", toString port, username]
      pure username
        where
          -- TODO: Don't hardcode this
          username = "holborn-test-user"

    -- | Create an empty postgres database owned by this user.
    createPostgresDatabase :: Port -> UserName -> IO DatabaseName
    createPostgresDatabase port username = do
      cmd "createdb" ["-p", toString port, "-O", username, database]
      pure database
        where
          -- TODO: Don't hardcode this
          database = "holborn-test-database"


    -- | Run SQL code in a file in the given database.
    --
    -- TODO: What should this return? Something that communicates results and how
    -- it got there.
    runSqlFromFile :: Postgres -> FilePath -> IO ()
    runSqlFromFile Running{port, username, database} filename = do
      cmd "psql" [ "-p", toString port
                 , "-U", username
                 , "-d", database
                 , "-qf", filename
                 ]

    toString :: Show a => a -> String
    toString = textToString . show


cmd :: FilePath -> [String] -> IO ()
cmd command args = do
  -- TODO: Capture output and only show if command fails.
  let command' = showCommandForUser command args
  callCommand command'


-- | Terminate a running Postgresql instance.
--
-- Blocks until the instance is definitely terminated.
stopPostgres :: Postgres -> IO ()
stopPostgres Running{directory} = do
  -- XXX: The documentation for terminateProcess says: "This function should
  -- not be used under normal circumstances - no guarantees are given
  -- regarding how cleanly the process is terminated." Does that mean we
  -- should do our own signalling, sending SIGTERM first, waiting, then
  -- sending SIGKILL if it's still going?
  cmd "pg_ctl" ["stop", "-D", directory, "-s", "-w", "-m", "fast"]
