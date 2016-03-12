{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Browse Git repository

module Holborn.Repo.Browse (BrowseAPI, browseAPI, codeBrowser) where

import BasicPrelude

import Control.Error (bimapExceptT)
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.ByteString.Lazy (fromStrict)
import Servant ((:>), (:<|>)(..), Capture, Get, Proxy(..), QueryParam, ServantErr(..), Server, JSON)
import Servant.Common.Text (FromText(..))
import Servant.HTML.Blaze (HTML)
import Servant.Server (enter, (:~>)(..), err500)

import Holborn.Repo.GitLayer ( Blob
                             , Commit
                             , GitException(..)
                             , Repository
                             , Revision
                             , Tree
                             , getBlob
                             , getTree
                             , notImplementedYet
                             , withRepository
                             )
import Holborn.ServantExtensions (CaptureAll)
import Holborn.JSON.RepoMeta (RepoMeta(..))

-- | The author of a commit
type Author = Text


-- XXX: Where we have "revspec" to catch a single segment as a Revision,
-- Github will accept a single segment, but will also accept, e.g.
-- 'refs/heads/master'.

type BrowseAPI =
  -- just the root
  Get '[JSON, HTML] RepoMeta

  -- e.g. /v1/repos/src/pulp/blob/master/setup.py
  :<|> "blob" :> Capture "revspec" Revision :> CaptureAll "pathspec" :> Get '[HTML] Blob
  -- e.g. /v1/repos/src/pulp/tree/master/
  :<|> "tree" :> Capture "revspec" Revision :> CaptureAll "pathspec" :> Get '[HTML] Tree
  :<|> "commits" :> Capture "revspec" Revision :> QueryParam "author" Author :> Get '[HTML] [Commit]
  :<|> "commit" :> Capture "revspec" Revision :> Get '[HTML] Commit


browseAPI :: Proxy BrowseAPI
browseAPI = Proxy


type RepoBrowser = ExceptT GitException IO


-- | Translate from RepoBrowser to the standard Servant monad
--
-- See http://haskell-servant.github.io/tutorial/server.html#using-another-monad-for-your-handlers
gitBrowserT :: ExceptT GitException IO :~> EitherT ServantErr IO
gitBrowserT = Nat (exceptTToEitherT . bimapExceptT gitExceptionToServantErr id)
  where
    exceptTToEitherT = EitherT . runExceptT

    gitExceptionToServantErr e = err500 { errBody = fromStrict (encodeUtf8 (show e)) }


codeBrowser :: Repository -> Server BrowseAPI
codeBrowser repo = enter gitBrowserT $
  -- XXX: What should we do for repos that don't have a master?
  renderTree repo master []
  :<|> renderBlob repo
  :<|> renderTree repo
  :<|> renderCommits repo
  :<|> renderCommit repo
  where
    master = fromMaybe (terror "Impossible!") (fromText "master")


renderBlob :: Repository -> Revision -> [Text] -> RepoBrowser Blob
renderBlob repo revision segments =
  fromMaybe (terror "no blob found") <$> withRepository repo (getBlob revision segments)


renderTree :: Repository -> Revision -> [Text] -> RepoBrowser Tree
renderTree repo revision segments =
  fromMaybe (terror "no tree found") <$> withRepository repo (getTree revision segments)


renderCommits :: Repository -> Revision -> Maybe Author -> RepoBrowser [Commit]
renderCommits = notImplementedYet "render commits"

renderCommit :: Repository -> Revision -> RepoBrowser Commit
renderCommit = notImplementedYet "render commit"
