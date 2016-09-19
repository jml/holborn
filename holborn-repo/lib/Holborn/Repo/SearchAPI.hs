{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Holborn.Repo.SearchAPI (SearchAPI, searchAPI, searchServer, SearchResultRow(..)) where

import HolbornPrelude

import Control.Error (bimapExceptT)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Servant ((:>), Get, Proxy(..), QueryParam, ServantErr(..), Server, JSON)
import Servant.Server (enter, (:~>)(..), err500)
import Data.Aeson (ToJSON)

import Holborn.ServantTypes (RenderedJson)
import Holborn.Repo.GitLayer (Repository, Revision, withRepository, BrowseException, Blob, BrowseException(..), getBlob)
import qualified Holborn.Repo.Search as S
import GHC.Generics (Generic)
import Data.List (groupBy)
import qualified Data.Text as T
import Web.HttpApiData (FromHttpApiData(..))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace
import Text.Blaze (ToMarkup(..))
import Text.Blaze.Renderer.Text (renderMarkup)
import qualified Data.Text.Lazy as TL

type GitPath = Text

data SearchResultRow = SearchResultRow
  { path :: GitPath
  , rendered :: TL.Text
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

type SearchAPI =
  -- q for query, after for pagination
  QueryParam "q" Text :> QueryParam "after" Text :> Get '[JSON] [SearchResultRow]


searchAPI :: Proxy SearchAPI
searchAPI = Proxy


-- See http://haskell-servant.github.io/tutorial/server.html#using-another-monad-for-your-handlers
searchT :: ExceptT BrowseException IO :~> ExceptT ServantErr IO
searchT = Nat (bimapExceptT searchExceptionToServantErr HolbornPrelude.id)
  where
    searchExceptionToServantErr _ = err500 { errBody = "invalid query" }


searchServer :: Repository -> Server SearchAPI
searchServer repo = enter searchT $
  searchHandler repo


searchHandler :: Repository -> Maybe Text -> Maybe Text -> ExceptT BrowseException IO  [SearchResultRow]
searchHandler repo q after = do
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
