{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Browse Git repository

module Holborn.Repo.Browse (API, server) where

import HolbornPrelude hiding (id)
import qualified HolbornPrelude (id)

import Control.Error (bimapExceptT)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.ByteString.Lazy (fromStrict)
import Servant ((:>), (:<|>)(..), Capture, CaptureAll, Get, QueryParam, ServantErr(..), Server, JSON, MimeRender(mimeRender))
import Servant.HTML.Blaze (HTML)
import Servant.Server (enter, (:~>)(..), err500, err404)
import Data.Aeson (encode)

import Holborn.ServantTypes (RenderedJson)
import Holborn.Repo.GitLayer ( Blob
                             , Commit
                             , BrowseException(..)
                             , Repository
                             , Revision
                             , Tree
                             , getBlob
                             , getTree
                             , notImplementedYet
                             , withRepository
                             , fillRepoMeta
                             )
import Holborn.JSON.RepoMeta (RepoMeta(..))

-- | The author of a commit
type Author = Text


-- XXX: Where we have "revspec" to catch a single segment as a Revision,
-- Github will accept a single segment, but will also accept, e.g.
-- 'refs/heads/master'.

type API =
  -- e.g. /v1/repos/src/pulp/blob/master/setup.py
  Get '[HTML, JSON, RenderedJson] RepoMeta
  :<|> "git" :> "blobs" :> Capture "revspec" Revision :>
    CaptureAll "pathspec" Text :> Get '[HTML, JSON, RenderedJson] Blob

  -- e.g. /v1/repos/src/pulp/tree/master/
  :<|> "git" :> "trees" :> Capture "revspec" Revision :>
    CaptureAll "pathspec" Text :> Get '[HTML, JSON, RenderedJson] Tree

  :<|> "commits" :> Capture "revspec" Revision :> QueryParam "author" Author :> Get '[HTML] [Commit]
  :<|> "git" :> "commits" :> Capture "revspec" Revision :> Get '[HTML] Commit


type RepoBrowser = ExceptT BrowseException IO


-- | Translate from RepoBrowser to the standard Servant monad
--
-- See http://haskell-servant.github.io/tutorial/server.html#using-another-monad-for-your-handlers
gitBrowserT :: ExceptT BrowseException IO :~> ExceptT ServantErr IO
gitBrowserT = Nat (bimapExceptT browseExceptionToServantErr HolbornPrelude.id)
  where
    browseExceptionToServantErr (GitException e) = err500 { errBody = fromStrict (encodeUtf8 (show e)) }
    browseExceptionToServantErr BlobNotFoundException = err404 { errBody = "not found" }

    -- TOOD empty (no commit) repositories return
    -- TreeNotFoundException which is not great.
    browseExceptionToServantErr TreeNotFoundException = err404 { errBody = "not found" }
    browseExceptionToServantErr CommitNotFoundException = err404 { errBody = "not found" }
    browseExceptionToServantErr RepoNotFoundException = err404 { errBody = "repo not found" }


server :: Repository -> Server API
server repo = enter gitBrowserT $
  renderMeta repo
  :<|> renderBlob repo
  :<|> renderTree repo
  :<|> renderCommits repo
  :<|> renderCommit repo


renderBlob :: Repository -> Revision -> [Text] -> RepoBrowser Blob
renderBlob repo revision segments = do
  x <- withRepository repo (getBlob revision segments)
  case x of
    Nothing -> throwE BlobNotFoundException
    Just x' -> pure x'


renderTree :: Repository -> Revision -> [Text] -> RepoBrowser Tree
renderTree repo revision segments = do
  x <- withRepository repo (getTree revision segments)
  case x of
    Nothing -> throwE TreeNotFoundException
    Just x' -> pure x'


renderCommits :: Repository -> Revision -> Maybe Author -> RepoBrowser [Commit]
renderCommits = notImplementedYet "render commits"

renderCommit :: Repository -> Revision -> RepoBrowser Commit
renderCommit = notImplementedYet "render commit"



instance MimeRender RenderedJson RepoMeta where
   mimeRender _ = encode


instance MimeRender RenderedJson Tree where
   mimeRender _ = encode


renderMeta :: Repository -> RepoBrowser RepoMeta
renderMeta repo = do
  x <- withRepository repo fillRepoMeta
  pure x
