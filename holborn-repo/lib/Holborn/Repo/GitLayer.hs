{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Git.Types (IsOid(renderObjOid))
import Git.Libgit2 (LgRepo, MonadLg, lgFactory)
import Text.Blaze (ToMarkup(..))
import Data.Aeson (ToJSON(..), genericToJSON, defaultOptions, object)
import Data.Aeson.Types (Options(fieldLabelModifier, omitNothingFields))
import GHC.Generics (Generic)
import Data.Conduit.Combinators (sinkList)
import qualified Data.Conduit.List as CL
import Data.Conduit (Conduit, runConduit, yield, awaitForever, (=$=), await)

-- XXX: Putting web stuff here to avoid orphan warnings. Would be nice to have
-- it completely separate.
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))

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
                Git.BlobEntry _blobEntryOid _blobEntryKind -> notImplementedYet "redirect to blob?"
                Git.CommitEntry _ -> notImplementedYet "what's a commit within tree?"

  where

    resolveReference = Git.resolveReference . revisionReference


-- XXX: Quickcheck this
segmentsToPath :: [Text] -> Git.TreeFilePath
segmentsToPath = encodeUtf8 . intercalate "/"


pathToSegments :: Git.TreeFilePath -> [Text]
pathToSegments = Text.splitOn "/" . decodeUtf8


data Blob = Blob { _gitBlobOid :: Git.BlobOid GitRepo
                 , blobContents :: ByteString  -- XXX: gitlib goes to some lengths (via Conduit) to allow for nice loading of this. Here, we just blat everything into memory. Tom comment: might be able to use https://hackage.haskell.org/package/pipes-aeson
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
  liftIO $ print oid
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
                  -- Comment tom: limit to e.g. 10MB for API here, and do the rest with a
                  -- different "raw" server like github.
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
instance FromHttpApiData Revision where
  parseUrlPiece revspec = pure (Revision $ "refs/heads/" ++ revspec)

instance ToHttpApiData Revision where
  toUrlPiece (Revision revision) =
    fromMaybe revision (Text.stripPrefix "refs/heads/" revision)


-- XXX: Is there a thing struggling to get out which is a combination of:
-- * repository
-- * revision
-- * path within repository
-- * realized _thing_

data TreeEntryMetaMode = SymlinkMode | BlobMode | ExecutableBlobMode | TreeMode deriving (Show)

-- TODO probably better off in common types
data TreeEntryMeta = TreeEntryMeta
    { _TreeEntryMeta_path :: Text
    , _TreeEntryMeta_mode :: TreeEntryMetaMode
    , _TreeEntryMeta_type_ :: Text -- TODO better types + ToJSON instances
    , _TreeEntryMeta_size :: Maybe Int -- doesn't apply to e.g. directories ATM
    , _TreeEntryMeta_sha :: Text
    } deriving (Show, Generic)

instance ToJSON TreeEntryMeta where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop (length ("_TreeEntryMeta_" :: String)), omitNothingFields = True }

instance ToJSON TreeEntryMetaMode where
    toJSON SymlinkMode = toJSON ("120000" :: Text)
    toJSON TreeMode = toJSON ("040000" :: Text)
    toJSON ExecutableBlobMode = toJSON ("100755" :: Text)
    toJSON BlobMode = toJSON ("100644" :: Text)


-- XXX: Do we want to parameterize this by GitRepo?
data Tree = Tree { _gitTree :: Git.Tree GitRepo
                 , gitTreeEntry :: Git.TreeEntry GitRepo
                 , gitEntries :: [(Git.TreeFilePath, TreeEntryMeta)]
                 , treeRevision :: Revision
                 , treePath :: [Text]
                 , treeRepository :: Repository
                 }


-- Not sure this encoder should be here.
-- https://developer.github.com/v3/git/trees/
instance ToJSON Tree where
    toJSON Tree{..} = object
      [ ("sha", toJSON (toUrlPiece treeRevision))
      , ("tree", toJSON (map snd gitEntries))
      , ("path", toJSON treePath)
      ]

instance ToJSON Blob where
    toJSON Blob{..} = object
      [ ("sha", toJSON (toUrlPiece blobRevision))
      , ("contents", toJSON (decodeUtf8 blobContents)) -- TODO deal with binary data (e.g. utf8-decoding fails)
      , ("path", toJSON blobPath)
      ]



-- XXX: loadTree is partial. If 'treeEntryOid' is not in the current
-- repo, then it will raise an exception.
--
-- NB lgSourceTreeEntries is recursive so we're loading the whole
-- repository here (gitlib doesn't give us any other option even
-- though the c-layer libgit2 allows us to selectively skip over
-- entries). TODO this is also super slow for large repos so we should
-- at the very least cache in process or find a faster way to
-- traverse.
loadTree :: (Git.MonadGit GitRepo m, MonadIO m) => Revision -> [Text] -> Repository -> Git.Tree GitRepo -> Git.TreeEntry GitRepo -> m Tree
loadTree revision segments repo tree entry = do
    -- Use conduits to compress a whole repository to sth readable in
    -- ~constant space.
    entriesMeta <- runConduit (Git.sourceTreeEntries tree =$= compressPaths =$= CL.mapM toTreeEntryMeta =$= sinkList)

    return (Tree tree entry (sortBy compareEntriesMeta entriesMeta) revision segments repo)

  where
    compareEntriesMeta (path1, TreeEntryMeta{_TreeEntryMeta_type_=type1}) (path2, TreeEntryMeta{_TreeEntryMeta_type_=type2}) =
      case (type1, type2) of
          ("tree", "tree") -> compare path1 path2
          ("tree", _) -> LT
          (_, "tree") -> GT
          _ -> compare path1 path2

    -- TODO: compresses empty paths like
    --   a/
    --   a/b/
    --   a/b/c/
    -- to just
    --   a/b/c
    -- unfortunately not that easy to do with the depth-first
    -- structure we're getting here...
    -- Right now we only keep the top level files, i.e. if a file is in a
    -- subtree (a/README.md) we're not keeping it.
    compressPaths = awaitForever $ \(path, entry) -> do
        -- Skip nested paths (length > 1)
        case pathToSegments path of
            [_] -> case entry of
                _ -> void (yield (path, entry))
            _ -> pure ()

    toTreeEntryMeta (path, entry) = case entry of
        Git.TreeEntry treeEntryOid -> treeMeta path treeEntryOid
        Git.BlobEntry _blobEntryOid _blobEntryKind -> blobMeta path _blobEntryOid _blobEntryKind
        Git.CommitEntry e -> notImplementedYet "what's a commit within tree?"

    blobMeta path _blobEntryOid _blobEntryKind = do
        -- blob <- Git.lookupBlob _blobEntryOid -- left commented out as documentation for later
        let mode = case _blobEntryKind of
                Git.PlainBlob -> BlobMode
                Git.ExecutableBlob -> ExecutableBlobMode
                Git.SymlinkBlob -> SymlinkMode
        let sha = renderObjOid _blobEntryOid
        pure $ (path, TreeEntryMeta
          { _TreeEntryMeta_path = decodeUtf8 path
          , _TreeEntryMeta_mode = mode
          , _TreeEntryMeta_type_ = "blob"
          , _TreeEntryMeta_size = Just 100 -- TODO replace fake data
          , _TreeEntryMeta_sha = sha
          })
    treeMeta path _treeOid = do
        -- tree <- Git.lookupTree _treeOid -- left commented out as documentation for later
        let sha = renderObjOid _treeOid
        pure $ (path, TreeEntryMeta
          { _TreeEntryMeta_path = decodeUtf8 path
          , _TreeEntryMeta_mode = TreeMode
          , _TreeEntryMeta_type_ = "tree"
          , _TreeEntryMeta_size = Nothing
          , _TreeEntryMeta_sha = sha
          })

-- XXX: The URL handling is awful. Not composable, assumes a certain path (by
-- returning absolute URLs and encoding the path *to* the repo as well as the
-- path *in* the repo). Really we should be using some sort of
-- Servant-provided URL generation.
urlWithinTree :: Tree -> Text -> TreeEntryMeta -> Text
urlWithinTree Tree{treeRepository = Repo{_repoName, _repoOwner}, treeRevision} basePath TreeEntryMeta{..} =
  intercalate "/"
    [ _repoOwner
    , _repoName
    , case _TreeEntryMeta_type_ of
          "tree" -> "git/trees"
          "blob" -> "git/blobs"
          "commit" -> "git/commits"
    , toUrlPiece treeRevision
    , basePath
    , _TreeEntryMeta_path
    ]


instance ToMarkup Tree where
  toMarkup tree@Tree{gitEntries, treePath} = do
    H.ul $ mapM_ renderEntry gitEntries

    where
      renderEntry (_, meta@TreeEntryMeta{_TreeEntryMeta_path}) =
        H.li $ makeLink (urlWithinTree tree currentPath meta) _TreeEntryMeta_path

      currentPath = intercalate "/" treePath
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
