-- | Governs how we map from repository IDs to files on disk.
--
-- We use 'Config' to turn 'RepoIds' received from clients into opaque
-- 'DiskLocation' values that can then be used to create, fork, and open
-- repositories.
--
-- ** Lazy initalization
--
-- We are planning on implementing a feature such that we will initialize
-- user's repositories for them, rather than them having to initialize them on
-- our servers.
--
-- This will happen in two contexts:
-- 1/ user pushes to a new repository
-- 2/ user does an implicit fork (communicated by API or the SSH header)
--
-- At the user level, an implicit fork happens when they push to a public
-- repository that they don't have write access to.
--
-- We care about the difference because a lazy init for an implicit
-- fork can be made much more efficient by cloning the other repo
-- first.
--
-- Note that holborn-repo has no notion of access. The contract is that by the
-- time a push arrives at our doorstep either API or the SSH terminator have
-- vetted that the push is valid. See note[1] at end of this file for why
-- we're not locking.

module Holborn.Repo.Filesystem
  ( DiskLocation
  , diskLocationToPath
  , getLocation
  , repoInit
  , forkInit
  ) where

import HolbornPrelude
import System.Process (proc, readCreateProcessWithExitCode, CreateProcess(cwd))
import System.Exit (ExitCode(..))
import Data.Text (unpack)

import Holborn.CommonTypes.Repo (RepoId)
import Holborn.Repo.Config (Config(..))
import Web.HttpApiData (ToHttpApiData(..))


-- | Opaque data type to describe where the repository lives on disk. Will
-- probably be extended to handle implicit clones.
data DiskLocation = DiskLocation { repoRoot :: FilePath, repoId :: RepoId }

-- | Get the disk location for a repository.
getLocation :: Config -> RepoId -> DiskLocation
getLocation Config{repoRoot} repoId = DiskLocation repoRoot repoId

-- | Get the absolute path on disk that a DiskLocation corresponds to.
diskLocationToPath :: DiskLocation -> String
diskLocationToPath DiskLocation{..} = repoRoot <> "/" <> textToString (toUrlPiece repoId)

-- | Initialize a bare Git repository at the given location.
repoInit :: DiskLocation -> IO (Maybe ())
repoInit DiskLocation{..} = do
    -- http://git-scm.com/docs/git-init
    -- "Running git init in an existing repository is safe.
    -- It will not overwrite things that are already there.
    -- The primary reason for rerunning git init is to pick up newly added templates."
    let init' = (proc "git" ["init", "--quiet", "--bare", unpack (show repoId)]) { cwd = Just repoRoot }

    -- TODO what do we do with stderr and stdout. Logging directly to
    -- syslog makes us lose structure (due to newlines), so we
    -- probably want some structured logging here?
    (exitCode, _, _) <- readCreateProcessWithExitCode init' ""
    pure $ case exitCode of
      ExitSuccess -> Just ()
      _ -> Nothing


-- | Not used yet but added so I don't lose my research into git
-- behaviour.
forkInit :: DiskLocation -> RepoId -> IO (Maybe DiskLocation)
forkInit (DiskLocation repoRoot existingRepoId) repoId = do
    let clone' = (proc "git" ["clone", "--bare", "--local", unpack (show existingRepoId), unpack (show repoId)]) { cwd = Just repoRoot }
    (exitCode, _, _) <- readCreateProcessWithExitCode clone' ""
    pure $ case exitCode of
      ExitSuccess -> Just (DiskLocation repoRoot repoId)
      _ -> Nothing



{-
[1] Experiment showing why we can get away without locking in our
code: git locks for us:

$ yes /tmp/x | head -n10 | xargs -P 10 -I{} git init --bare "{}"
error: Unable to open /tmp/x/HEAD.lock for writing
fatal: cannot mkdir /tmp/x: File exists
error: could not lock config file /tmp/x/config: File exists
Reinitialized existing Git repository in /tmp/x/
Reinitialized existing Git repository in /tmp/x/
fatal: cannot copy '/nix/store/bf8nzsz9mslhkrqdfg9ijkim8x9wj68r-git-2.3.5/share/git-core/templates/hooks/post-update.sample' to '/tmp/x/hooks/post-update.sample': File exists
error: could not lock config file /tmp/x/config: File exists
fatal: cannot copy '/nix/store/bf8nzsz9mslhkrqdfg9ijkim8x9wj68r-git-2.3.5/share/git-core/templates/hooks/prepare-commit-msg.sample' to '/tmp/x/hooks/prepare-commit-msg.sample': File exists
Initialized empty Git repository in /tmp/x/
error: could not lock config file /tmp/x/config: File exists
error: could not lock config file /tmp/x/config: File exists
error: could not lock config file /tmp/x/config: File exists
error: could not lock config file /tmp/x/config: File exists
Reinitialized existing Git repository in /tmp/x/
Reinitialized existing Git repository in /tmp/x/
Reinitialized existing Git repository in /tmp/x/
-}
