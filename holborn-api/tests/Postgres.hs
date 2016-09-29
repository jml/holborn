-- | Make & destroy Postgresql instances for testing.
--
-- Deliberately doesn't have any code specific to Holborn, so we can re-use it
-- in other places.

module Postgres
  ( Postgres
  , connection
  , makeDatabase
  , resetPostgres
  , stopPostgres
  ) where

import HolbornPrelude

import Data.Word (Word16)
import Database.PostgreSQL.Simple (ConnectInfo(..), defaultConnectInfo)
import System.Exit (ExitCode(..))
import System.IO.Error (userError)
import qualified System.Process as P
import System.Posix.Temp (mkdtemp)

-- | A TCP port.
type Port = Word16

-- | Postgres username.
type UserName = String

-- | A Postgres database that we can use for tests.
--
-- Create with 'makeDatabase', stop with 'stopPostgres', and get a connection
-- to the database using 'connection'.
data Postgres =
  Postgres { directory :: FilePath
           , connection :: ConnectInfo
           }

-- | Make sure we have a database made of the SQL that's in the given file.
makeDatabase :: UserName -> FilePath -> IO Postgres
makeDatabase username schema = do
  -- TODO: Try to re-use existing database directory, somehow
  pgDir <- initDb
  port <- launchPostgres pgDir
  onException (createPostgresUser pgDir port username) (stopPostgres' pgDir)
  -- Set the template database to be whatever is in the provided schema. Then,
  -- when we drop & create databases to reset them, they'll automatically have
  -- the schema loaded in template1.
  --
  -- See https://www.postgresql.org/docs/9.5/static/manage-ag-templatedbs.html
  let conn = defaultConnectInfo { connectPort = port
                                , connectUser = username
                                , connectHost = pgDir
                                }
  onException (runSqlFromFile (conn { connectDatabase = "template1" }) schema) (stopPostgres' pgDir)
  conn' <- onException (createPostgresDatabase conn) (stopPostgres' pgDir)
  pure $ Postgres { directory = pgDir
                  , connection = conn'
                  }

  where
    -- | Initialize a Postgresql directory.
    initDb :: IO FilePath
    initDb = do
      dir <- mkdtemp "/tmp/holborn-postgres"
      pg_ctl dir "init" []
      -- Without this, Debian & Ubuntu will try to write to
      -- /var/run/postgresql, which is for privileged users only.
      appendFile (dir <> "/postgresql.conf") ("unix_socket_directories = '" <> fromString dir <> "'\n")
      -- XXX: Assuming socket dir == db dir
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
    createPostgresUser :: FilePath -> Port -> UserName -> IO ()
    createPostgresUser socketDir port user =
      cmd "createuser" ["-p", toString port, "-h", socketDir, user]

    -- | Create an empty postgres database owned by this user.
    --
    -- Takes a ConnectInfo but ignores the 'connectDatabase' field. Returns a
    -- new ConnectInfo with 'connectDatabase' set to the name of the
    -- newly-created database.
    createPostgresDatabase :: ConnectInfo -> IO ConnectInfo
    createPostgresDatabase connection = do
      let newConnection = connection { connectDatabase = database }
      createDatabase' newConnection
      pure newConnection
        where
          -- TODO: Don't hardcode this
          database = "holborn-test-database"


    -- | Run SQL code in a file in the given database.
    --
    -- TODO: What should this return? Something that communicates results and how
    -- it got there.
    runSqlFromFile :: ConnectInfo -> FilePath -> IO ()
    runSqlFromFile ConnectInfo{..} filename = do
      cmd "psql" [ "-p", toString connectPort
                 , "-U", connectUser
                 , "-d", connectDatabase
                 , "-h", connectHost
                 , "-qf", filename
                 ]


-- | Execute a command with arguments.
cmd :: FilePath -> [String] -> IO ()
cmd command args = do
  -- TODO: Show output & error only if command fails. readProcessWithExitCode
  -- should be ideal for this, but there's some weird interaction it tries to
  -- get stdout from `pg_ctl`.
  let spec = (P.proc command args) { P.std_out = P.CreatePipe
                                   , P.std_err = P.CreatePipe
                                   }
  (_, _, _, p) <- P.createProcess spec
  exitCode <- P.waitForProcess p
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> processFailed exitCode

  where
    processFailed exitCode = ioError (userError (
      "Process failed: " <> P.showCommandForUser command args <> "(" <> toString exitCode <> ")\n"))


-- | Execute the 'pg_ctl' command via the shell.
--
-- Blocks until command successfully completes, and uses the quietest possible
-- output.
pg_ctl :: FilePath -> String -> [String] -> IO ()
pg_ctl pgDir command args = do
  cmd "pg_ctl" $ [command, "-D", pgDir, "-s", "-w"] <> args


-- | Primitive for creating a database in a running Postgres instance.
createDatabase' :: ConnectInfo -> IO ()
createDatabase' ConnectInfo{..} = cmd "createdb" ["-p", toString connectPort, "-h", connectHost, "-O", connectUser, connectDatabase]

-- | Primitive for dropping a database in a running Postgres instance.
dropDatabase' :: ConnectInfo -> IO ()
dropDatabase' ConnectInfo{..} = cmd "dropdb" ["-p", toString connectPort, "-U", connectUser, "-h", connectHost, connectDatabase]

-- | Reset the postgres instance to the initial schema.
resetPostgres :: Postgres -> IO ()
resetPostgres Postgres{connection = connection@(ConnectInfo {connectPort,connectUser,connectDatabase,connectHost})} = do
  runSql "SELECT pid, (SELECT pg_terminate_backend(pid)) as killed from pg_stat_activity WHERE state LIKE 'idle';"
  dropDatabase' connection
  -- Uses schema set in "template1" database. See comment in makeDatabase.
  createDatabase' connection
  where
    runSql query = do
      cmd "psql" [ "-p", toString connectPort
                 , "-U", connectUser
                 , "-d", connectDatabase
                 , "-h", connectHost
                 , "-q"
                 , "-c", query
                 ]

-- TODO: Move to HolbornPrelude
toString :: Show a => a -> String
toString = textToString . show

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
