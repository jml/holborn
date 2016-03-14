{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-|
The Holborn interface to Git.
-}
module Holborn.Repo.GitLayer
       ( Blob
       , Commit
       , Git.GitException(..)
       , Repository
       , Revision
       , Tree
       , getBlob
       , getTree
       , makeRepository
       , notImplementedYet
       , withRepository
       , fillRepoMeta
       ) where

import BasicPrelude
import Control.Error (bimapExceptT, syncIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Except (ExceptT)
import Data.Tagged (Tagged(..))
import qualified Data.Text as Text
import qualified Git
import Git.Libgit2 (LgRepo, MonadLg, lgFactory)
import Text.Blaze (ToMarkup(..))
import Data.Aeson (ToJSON(..), genericToJSON, Value(..), object)

-- XXX: Putting web stuff here to avoid orphan warnings. Would be nice to have
-- it completely separate.
import Servant.Common.Text (FromText(..), ToText(..))
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- XXX: Get the instances. Need to move all HTML formatting stuff to a
-- separate module, and leave this just about using Git.
import Holborn.Repo.HtmlFormatTokens ()
import Holborn.JSON.RepoMeta (RepoMeta(..))
import Holborn.Syntax (annotateCode)

-- | A git repository
data Repository =
  Repo { _repoOwner :: Text
       , _repoName :: Text
       , _repoPath :: FilePath
       } deriving (Eq, Show)


-- | The underlying Git repository.
type GitRepo = LgRepo
type GitM m = ReaderT GitRepo m


-- XXX: Make this a sum type of GitException plus our own kinds of errors.
type GitException = Git.GitException


makeRepository :: Text -> Text -> FilePath -> Repository
makeRepository = Repo


-- TODO: Verify behaviour when the repository does not exist.
-- XXX: Store "Repository" in Reader(T)
withRepository :: Repository -> (Repository -> GitM IO a) -> ExceptT GitException IO a
withRepository repo action =
  bimapExceptT justGitException id $ syncIO $ Git.withRepository' lgFactory options (action repo)
  where
    options = Git.RepositoryOptions { Git.repoPath = _repoPath repo
                                    , Git.repoWorkingDir = Nothing
                                    , Git.repoIsBare = False  -- XXX: Make this an option
                                    , Git.repoAutoCreate = False
                                    }

    justGitException :: SomeException -> GitException
    justGitException e =
      case fromException e of
        Just gitException -> gitException
        _ -> terror $ "Unknown error: " ++ show e


-- XXX: Staircasing
getTree :: (MonadLg io) => Revision -> [Text] -> Repository -> GitM io (Maybe Tree)
getTree revision segments repo = do
  oid <- resolveReference revision
  case oid of
    Nothing -> return Nothing
    Just oid' -> do
      commit <- Git.lookupCommit (Tagged oid')
      let treeOid = Git.commitTree commit
      tree <- Git.lookupTree treeOid
      case segments of
        -- XXX: Aka, root.
        [] -> do
          tree' <- loadTree revision segments repo tree (Git.TreeEntry treeOid)
          return $ Just tree'
        _ -> do
          entry <- Git.treeEntry tree (segmentsToPath segments)
          case entry of
            Nothing -> return Nothing
            Just entry' ->
              case entry' of
                Git.TreeEntry treeEntryOid -> do
                  subtree <- Git.lookupTree treeEntryOid
                  tree' <- loadTree revision segments repo subtree entry'
                  return $ Just tree'
                Git.BlobEntry _blobEntryOid _blobEntryKind -> notImplementedYet "redirect to blob"
                Git.CommitEntry _ -> notImplementedYet "what's a commit within tree?"

  where

    resolveReference = Git.resolveReference . revisionReference


-- XXX: Quickcheck this
segmentsToPath :: [Text] -> Git.TreeFilePath
segmentsToPath = encodeUtf8 . intercalate "/"


pathToSegments :: Git.TreeFilePath -> [Text]
pathToSegments = Text.splitOn "/" . decodeUtf8


data Blob = Blob { _gitBlobOid :: Git.BlobOid GitRepo
                 , blobContents :: ByteString  -- XXX: gitlib goes to some lengths (via Conduit) to allow for nice loading of this. Here, we just blat everything into memory.
                 , blobRevision :: Revision
                 , blobPath :: [Text]
                 , blobRepository :: Repository
                 }


instance ToMarkup Blob where
  toMarkup blob =
    case annotateCode filename contents of
      Left e -> do
        -- TODO: Handle unparseable files better. In the meantime, show our
        -- parse error so we can debug better.
        H.div $ do
          "Could not parse"
          H.pre $ toMarkup (show e)
        H.pre $ toMarkup (decodeUtf8 contents)
      Right annotated -> toMarkup annotated
    where
      filename = intercalate "/" (blobPath blob)
      contents = blobContents blob


-- XXX: Copy/paste of getTree, which is also terrible and staircasey.
getBlob :: (MonadLg io) => Revision -> [Text] -> Repository -> GitM io (Maybe Blob)
getBlob revision segments repo = do
  oid <- resolveReference revision
  case oid of
    Nothing -> return Nothing
    Just oid' -> do
      commit <- Git.lookupCommit (Tagged oid')
      let treeOid = Git.commitTree commit
      tree <- Git.lookupTree treeOid
      case segments of
        -- XXX: Aka, root.
        [] -> notImplementedYet "Not sure what to do for blob root"
        _ -> do
          entry <- Git.treeEntry tree (segmentsToPath segments)
          case entry of
            Nothing -> return Nothing
            Just entry' ->
              case entry' of
                Git.TreeEntry _treeEntryOid -> notImplementedYet "redirect to tree"
                Git.BlobEntry blobEntryOid blobEntryKind -> do
                  blob <- Git.lookupBlob blobEntryOid
                  -- XXX: Loads the entire blob into memory.
                  contents <- Git.blobToByteString blob
                  return $ Just $ Blob blobEntryOid contents revision segments repo
                Git.CommitEntry _ -> notImplementedYet "what's a commit within tree?"

  where
    resolveReference = Git.resolveReference . revisionReference


data Commit = Commit

instance ToMarkup Commit where
  toMarkup = undefined

instance ToMarkup [Commit] where
  toMarkup = undefined


-- XXX: We should rename this to Reference or something. Means either branch
-- name or rev sha or more or less anything that can be passed to `git
-- rev-parse`. jml really needs to learn Git terminology.
data Revision = Revision { revisionReference :: Text } deriving (Eq, Show)

-- TODO: Quickcheck tests for this

-- XXX: This is *hideously* wrong. More or less what we want to do is
-- implement the rules in "git rev-parse", preserving the thing asked for as
-- well as the "canonical" result.
instance FromText Revision where
  fromText revspec = Just (Revision $ "refs/heads/" ++ revspec)

instance ToText Revision where
  toText (Revision revision) =
    fromMaybe revision (Text.stripPrefix "refs/heads/" revision)


-- XXX: Is there a thing struggling to get out which is a combination of:
-- * repository
-- * revision
-- * path within repository
-- * realized _thing_


-- XXX: Do we want to parameterize this by GitRepo?
data Tree = Tree { _gitTree :: Git.Tree GitRepo
                 , gitTreeEntry :: Git.TreeEntry GitRepo
                 , gitEntries :: [(Git.TreeFilePath, Git.TreeEntry GitRepo)]
                 , treeRevision :: Revision
                 , treePath :: [Text]
                 , treeRepository :: Repository
                 }

-- TODO - Gitlib doesn't seem to give us all the information we need
-- like mode, last commit etc. for an entry. Needs work.
treeEntryToList entry@(Git.BlobEntry oid kind) =
    [ ("type", "blob")
    ]
treeEntryToList entry@(Git.TreeEntry oid) =
    [ ("type", "tree")
    ]
treeEntryToList entry@(Git.CommitEntry oid) =
    [ ("type",  "commit")
    ]

-- Not sure this encoder should be here.
-- https://developer.github.com/v3/git/trees/
instance ToJSON Tree where
    toJSON Tree{..} = object
      [ ("sha",  String (show treeRevision))
        -- TODO add mode, type, sha, size
      , ("tree", toJSON (map (\(path, entry) -> object [("path", toJSON (decodeUtf8 path))]) gitEntries) )
      , ("path", toJSON treePath)
      ]


-- XXX: This is partial. If 'treeEntryOid' is not in the current repo, then it will raise an exce
loadTree :: Git.MonadGit GitRepo m => Revision -> [Text] -> Repository -> Git.Tree GitRepo -> Git.TreeEntry GitRepo -> m Tree
loadTree revision segments repo tree entry = do
  entries <- Git.listTreeEntries tree
  return $ Tree tree entry entries revision segments repo

-- XXX: The URL handling is awful. Not composable, assumes a certain path (by
-- returning absolute URLs and encoding the path *to* the repo as well as the
-- path *in* the repo). Really we should be using some sort of
-- Servant-provided URL generation.

-- XXX: 'kind' is entry type, either "tree" or "blob". Really, we should have
-- a much better URL handling system.
treeURL :: Tree -> Text -> Text
treeURL tree kind = intercalate "/" (["", owner, name, kind, revspec] ++ treePath tree)
  where
    owner = _repoOwner repo
    name = _repoName repo
    repo = treeRepository tree
    revspec = toText (treeRevision tree)


urlWithinTree :: Tree -> (Git.TreeFilePath, Git.TreeEntry r) -> Text
urlWithinTree tree (path, entry) = intercalate "/" [basePath, decodeUtf8 path]
  where
    basePath =
      case entry of
        Git.TreeEntry _ -> treeURL tree "tree"
        Git.BlobEntry _blobEntryOid _blobEntryKind -> treeURL tree "blob"
        Git.CommitEntry _ -> notImplementedYet "what's a commit within tree?"


instance ToMarkup Tree where
  toMarkup tree = do
    H.h1 $ makeLink (urlWithinTree tree (encodeUtf8 currentPath, gitTreeEntry tree)) currentPath
    H.ul $ mapM_ renderEntry (gitEntries tree)

    where
      renderEntry treeEntry = H.li $ makeLink (urlWithinTree tree treeEntry) (decodeUtf8 (fst treeEntry))

      currentPath = intercalate "/" (treePath tree)
      makeLink url path = H.a ! A.href (H.toValue ("/v1/repos/" <> url)) $ toMarkup path


-- XXX: Move this to some more common library
notImplementedYet :: Text -> a
notImplementedYet feature = terror $ "Not implemented yet: " ++ feature


-- | Fill in information about the repository we want to render or
-- show. E.g. languages, number of commits, public URL, ssh URL, etc.
--
-- For now it has access to the repository itself to run git commands
-- but it'll likely be more efficient to cache the metadata on push
-- (which is much rarer than reading).
fillRepoMeta :: Repository -> GitM IO RepoMeta
fillRepoMeta Repo{..} =
  return $ RepoMeta _repoOwner _repoName 10 11 12 -- Fake data 10 11 12

instance ToMarkup RepoMeta where
  toMarkup RepoMeta{..} = do
    H.h1 $ "debug rendering for root metadata"
    H.a ! A.href (H.toValue ("/v1/repos/" <> _RepoMeta_owner <> "/" <> _RepoMeta_repo <> "/tree/master")) $ "tree-root"
