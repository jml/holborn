{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Browse Git repository

module Holborn.Repo.Browse (BrowseAPI, browseAPI, codeBrowser) where

import BasicPrelude

import Control.Error (bimapExceptT)
import Control.Monad.Trans.Except (ExceptT)
import Data.ByteString.Lazy (fromStrict)
import Servant ((:>), (:<|>)(..), Capture, Get, Proxy(..), QueryParam, ServantErr(..), Server, JSON, MimeRender(mimeRender))
import Servant.HTML.Blaze (HTML)
import Servant.Server (enter, (:~>)(..), err500)
import Data.Aeson (encode)

import Holborn.ServantTypes (RenderedJson)
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
                             , fillRepoMeta
                             )
import Holborn.JSON.RepoMeta (RepoMeta(..))

-- | The author of a commit
type Author = Text


-- XXX: Where we have "revspec" to catch a single segment as a Revision,
-- Github will accept a single segment, but will also accept, e.g.
-- 'refs/heads/master'.

type CPS = Capture "pathspec" Text

type BrowseAPI =
  -- just the root
  Get '[HTML, JSON, RenderedJson] RepoMeta

  -- e.g. /v1/repos/src/pulp/blob/master/setup.py
  -- XXX: blob & tree have this hacky thing because Servant 0.5 broke our CaptureAll combinator.
  -- https://github.com/haskell-servant/servant/issues/257 tracks fixing this.
  :<|> "git" :> "blobs" :> Capture "revspec" Revision :>
    ((Get '[HTML, JSON, RenderedJson] Blob)
     :<|> (CPS :> Get '[HTML, JSON, RenderedJson] Blob)
     :<|> (CPS :> CPS :> Get '[HTML, JSON, RenderedJson] Blob)
     :<|> (CPS :> CPS :> CPS :> Get '[HTML, JSON, RenderedJson] Blob)
     :<|> (CPS :> CPS :> CPS :> CPS :> Get '[HTML, JSON, RenderedJson] Blob)
     :<|> (CPS :> CPS :> CPS :> CPS :> CPS :> Get '[HTML, JSON, RenderedJson] Blob))


  -- e.g. /v1/repos/src/pulp/tree/master/
  :<|> "git" :> "trees" :> Capture "revspec" Revision :>
    ((Get '[HTML, JSON, RenderedJson] Tree)
     :<|> (CPS :> Get '[HTML, JSON, RenderedJson] Tree)
     :<|> (CPS :> CPS :> Get '[HTML, JSON, RenderedJson] Tree)
     :<|> (CPS :> CPS :> CPS :> Get '[HTML, JSON, RenderedJson] Tree)
     :<|> (CPS :> CPS :> CPS :> CPS :> Get '[HTML, JSON, RenderedJson] Tree)
     :<|> (CPS :> CPS :> CPS :> CPS :> CPS :> Get '[HTML, JSON, RenderedJson] Tree))

  :<|> "commits" :> Capture "revspec" Revision :> QueryParam "author" Author :> Get '[HTML] [Commit]
  :<|> "git" :> "commits" :> Capture "revspec" Revision :> Get '[HTML] Commit


browseAPI :: Proxy BrowseAPI
browseAPI = Proxy


type RepoBrowser = ExceptT GitException IO


-- | Translate from RepoBrowser to the standard Servant monad
--
-- See http://haskell-servant.github.io/tutorial/server.html#using-another-monad-for-your-handlers
gitBrowserT :: ExceptT GitException IO :~> ExceptT ServantErr IO
gitBrowserT = Nat (bimapExceptT gitExceptionToServantErr id)
  where
    gitExceptionToServantErr e = err500 { errBody = fromStrict (encodeUtf8 (show e)) }


codeBrowser :: Repository -> Server BrowseAPI
codeBrowser repo = enter gitBrowserT $
  -- XXX: What should we do for repos that don't have a master?
  renderMeta repo
  :<|> (handlePaths . renderBlob repo)
  :<|> (handlePaths . renderTree repo)
  :<|> renderCommits repo
  :<|> renderCommit repo

  where handlePaths f = (f []) :<|> (\a -> f [a]) :<|> (\a b -> f [a, b]) :<|> (\a b c -> f [a, b, c]) :<|> (\a b c d -> f [a, b, c, d]) :<|> (\a b c d e -> f [a, b, c, d, e])


renderMeta :: Repository -> RepoBrowser RepoMeta
renderMeta repo = do
  x <- withRepository repo fillRepoMeta
  pure x


renderBlob :: Repository -> Revision -> [Text] -> RepoBrowser Blob
renderBlob repo revision segments =
  fromMaybe (terror ("no blob found for " <> (show segments))) <$> withRepository repo (getBlob revision segments)


renderTree :: Repository -> Revision -> [Text] -> RepoBrowser Tree
renderTree repo revision segments =
    fromMaybe (terror ("no tree found for" <> (show segments))) <$> withRepository repo (getTree revision segments)


renderCommits :: Repository -> Revision -> Maybe Author -> RepoBrowser [Commit]
renderCommits = notImplementedYet "render commits"

renderCommit :: Repository -> Revision -> RepoBrowser Commit
renderCommit = notImplementedYet "render commit"



instance MimeRender RenderedJson RepoMeta where
   mimeRender _ = encode


instance MimeRender RenderedJson Tree where
   mimeRender _ = encode
