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
import Servant ((:>), Get, QueryParam, ServantErr(..), Server, JSON)
import Servant.Server (enter, (:~>)(..), err400)
import Data.Aeson (ToJSON)

import Holborn.Repo.GitLayer (Repository, Revision, withRepository, BrowseException, Blob, BrowseException(..), getBlob)
import qualified Holborn.Repo.Search as S
import GHC.Generics (Generic)
import qualified Data.Text as T
import Web.HttpApiData (FromHttpApiData(..))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.Blaze (ToMarkup(..))
import Text.Blaze.Renderer.Text (renderMarkup)

type GitPath = Text

data SearchResultRow = SearchResultRow
  { path :: GitPath
  , rendered :: LText
  , lineNumbers :: [Integer] -- Context to show to be decided on client side
  } deriving (Show, Generic)

instance ToJSON SearchResultRow


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
  QueryParam "q" Text :> QueryParam "after" Text :> Get '[JSON] [SearchResultRow]


-- See http://haskell-servant.github.io/tutorial/server.html#using-another-monad-for-your-handlers
searchT :: ExceptT BrowseException IO :~> ExceptT ServantErr IO
searchT = Nat (bimapExceptT searchExceptionToServantErr HolbornPrelude.id)
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
      Right (master :: Revision) = parseUrlPiece "master"

  -- Extract all blobs (this can be cached heavily with the current
  -- commit).t
  blobs <- mapM (\p -> withRepository repo (getBlob master (pathSegments p))) paths
  let pathToBlob = Map.fromList (zip (map fromString paths) (map fromJust blobs))

  pure (matchesToResults pathToBlob matches)
