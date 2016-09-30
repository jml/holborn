{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Holborn.Repo.SearchAPI (API, server, SearchResultRow(..)) where

import HolbornPrelude

import Control.Error (bimapExceptT)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Servant ((:>), Get, QueryParam, ServantErr(..), Server, MimeRender(mimeRender))
import Servant.Server (enter, (:~>)(..), err400)
import Data.Aeson (ToJSON, encode)
import Holborn.ServantTypes (RenderedJson)
import Holborn.Repo.GitLayer (Repository, withRepository, BrowseException, Blob, BrowseException(..), getBlob, refMaster)
import qualified Holborn.Repo.Search as S
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.Blaze (ToMarkup(..))
import Text.Blaze.Renderer.Text (renderMarkup)
import GHC.Generics (Generic)

type GitPath = Text

data SearchResultRow = SearchResultRow
  { path :: GitPath
  , rendered :: LText
  , lineNumbers :: [Integer] -- Context to show to be decided on client side
  } deriving (Show, Generic)


instance ToJSON SearchResultRow

instance MimeRender RenderedJson SearchResultRow where
   mimeRender _ = encode

instance MimeRender RenderedJson [SearchResultRow] where
   mimeRender _ = encode

-- Transform a list of matches a renderable search result. This means
-- rendering a file with the syntax highlighter, snipping out the
-- relevant parts, etc.
matchesToResults :: Map.Map GitPath Blob -> [S.Match] -> [SearchResultRow]
matchesToResults blobMap matches' = map toRow matches'
  where
    toRow S.Match{..} = SearchResultRow (fromString path) (contents path) (map fst matches)
    contents path' = renderMarkup (toMarkup (blobMap Map.! (fromString path')))


-- | This is the per-repository search API which is different from the
-- API exposed by holborn-api.
type API =
  -- q for query, after for pagination
  QueryParam "q" Text :> QueryParam "after" Text :> Get '[RenderedJson] [SearchResultRow]


-- See http://haskell-servant.github.io/tutorial/server.html#using-another-monad-for-your-handlers
searchT :: ExceptT BrowseException IO :~> ExceptT ServantErr IO
searchT = Nat (bimapExceptT searchExceptionToServantErr identity)
  where
    searchExceptionToServantErr _ = err400 { errBody = "invalid query" }


server :: Repository -> Server API
server repo = enter searchT $
  searchHandler repo


searchHandler :: Repository -> Maybe Text -> Maybe Text -> ExceptT BrowseException IO  [SearchResultRow]
searchHandler repo q _after = do
  -- TODO correct error message, BrowseException not good here.
  search <- maybe (throwE BlobNotFoundException) pure (q >>= S.parseSearch)
  matches <- liftIO (S.runBasicSearch repo search)

  let paths = map S.path matches
      pathSegments = (T.splitOn "/") . fromString

  -- Extract all blobs (this can be cached heavily with the current
  -- commit).
  blobs <- traverse (\p -> withRepository repo (getBlob refMaster (pathSegments p))) paths
  let pathToBlob = Map.fromList (zip (map fromString paths) (map fromJust blobs))

  pure (matchesToResults pathToBlob matches)
