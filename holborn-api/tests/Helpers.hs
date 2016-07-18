-- | Support code for holborn-api integration tests.

module Helpers
  ( Postgres
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

-- | A Postgres database that we can use for tests.
--
-- Create with 'makeDatabase', stop with 'stopPostgres', and get a connection
-- to the database using 'connection'.
data Postgres =
  Postgres { directory :: FilePath
           , connection :: ConnectInfo
           }

-- | Make sure we have a database made of the SQL that's in the given file.
makeDatabase :: FilePath -> IO Postgres
makeDatabase schema = do
  -- TODO: Try to re-use existing database directory, somehow
  pgDir <- initDb
  -- TODO: Try to re-use the running instance?
  port <- launchPostgres pgDir
  user <- onException (createPostgresUser port) (stopPostgres' pgDir)
  database <- onException (createPostgresDatabase port user) (stopPostgres' pgDir)
  let conn = defaultConnectInfo { connectPort = port
                                , connectUser = user
                                , connectDatabase = database
                                }
  let postgres = Postgres { directory = pgDir
                          , connection = conn
                          }
  onException (runSqlFromFile postgres schema) (stopPostgres postgres)
  pure postgres

  where
    -- | Initialize a Postgresql directory.
    initDb :: IO FilePath
    initDb = do
      dir <- mkdtemp "/tmp/holborn-postgres"
      pg_ctl dir "init" []
      pure dir

    -- | Start a Postgresql instance using the given directory.
    --
    -- Blocks until the Postgresql instance is ready for use.
    launchPostgres :: FilePath -> IO Port
    launchPostgres pgDir = do
      -- TODO: Don't hardcode this.
      let port = 5444
      launchPostgres' pgDir port
      pure port

    -- | Start a Postgresql instance using the given directory.
    --
    -- Blocks until the Postgresql instance is ready for use.
    launchPostgres' :: FilePath -> Port -> IO ()
    launchPostgres' pgDir port = do
      let portOpt = "-p " <> textToString (show port)
      pg_ctl pgDir "start" ["-o", portOpt]

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
    runSqlFromFile Postgres{connection} filename = do
      cmd "psql" [ "-p", toString (connectPort connection)
                 , "-U", (connectUser connection)
                 , "-d", (connectDatabase connection)
                 , "-qf", filename
                 ]

    toString :: Show a => a -> String
    toString = textToString . show


-- | Execute a command with arguments via the shell.
cmd :: FilePath -> [String] -> IO ()
cmd command args = do
  -- TODO: Capture output and only show if command fails.
  let command' = showCommandForUser command args
  callCommand command'

-- | Execute the 'pg_ctl' command via the shell.
--
-- Blocks until command successfully completes, and uses the quietest possible
-- output.
pg_ctl :: FilePath -> String -> [String] -> IO ()
pg_ctl pgDir command args = do
  cmd "pg_ctl" $ [command, "-D", pgDir, "-s", "-w"] <> args


-- | Terminate a running Postgresql instance.
--
-- Blocks until the instance is definitely terminated.
stopPostgres :: Postgres -> IO ()
stopPostgres Postgres{directory} = stopPostgres' directory


-- | Terminate a running Postgresql instance.
--
-- Blocks until the instance is definitely terminated.
stopPostgres' :: FilePath -> IO ()
stopPostgres' directory = do
  pg_ctl directory "stop" ["-m", "fast"]
