{-| Web frontend to syntax analyzer.

Intended to provide:
  1. An API for other, user-facing frontends to call out to.
  2. A development server for iterating on syntax highlighting.

-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Holborn.Web
       ( SyntaxAPI
       , syntaxAPI
       , server
       ) where

import BasicPrelude

import Control.Monad.Except (throwError)
import Control.Monad.Trans.Either (EitherT)
import Servant
import Servant.HTML.Blaze
import System.Directory (getDirectoryContents)
import System.FilePath (joinPath)

import Text.Blaze (ToMarkup(..))
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- Get the typeclass instances for converting Holborn stuff to HTML.
import Holborn.HtmlFormat ()
import Holborn.Internal (FileType(..), getFileType)
import Holborn.Style (monokai)
import Holborn.Syntax ( HolbornSource
                      , annotatePythonCode
                      )


-- QUESTION: Can we compose configuration structures so that the
-- server-specific stuff (like PORT) is separate from the application-specific
-- stuff (like path to code)? Is it even worth it?

-- QUESTION: Should we have a Config object (and presumably ReaderT monad), or
-- should we just pass parameters around as needed?

-- TODO: We can probably get rid of 'demo' real soon now.
type SyntaxAPI =
       "demo"  :> Get '[HTML] HolbornSource
  :<|> "files" :> PathAPI


syntaxAPI :: Proxy SyntaxAPI
syntaxAPI = Proxy


data PathResource = DirResource Folder | FileResource HolbornSource

-- TODO: Is there a better type for this (e.g. one that ensures no slashes,
-- regular character set).
type PathSegment = Text

data Folder = Folder { _rootDir    :: FilePath
                           , _currentDir :: [PathSegment]
                           , _children   :: [PathSegment]
                           }


makeFolder :: FilePath -> [PathSegment] -> IO Folder
makeFolder rootDirectory segments = do
  children <- getDirectoryContents (rootDirectory </> joinSegments segments)
  return $ Folder rootDirectory segments [fromString child | child@(x:_) <- children, x /= '.']


folderChildren :: Folder -> [PathSegment]
folderChildren = _children


folderPath :: Folder -> FilePath
folderPath dir = (_rootDir dir) </> joinSegments (_currentDir dir)


-- XXX: Ideally we'd use safe links here, but we need to de-horribilify the
-- API types first.
childURL :: Folder -> PathSegment -> Text
childURL dir segment = intercalate "/" (["", "files"] ++ _currentDir dir ++ [segment])


-- TODO: Crappy, temporary API. We actually want to allow for multiple path
-- segments, but that requires some mucking around with Servant which is more
-- than we want to do right now. We've chosen max depth of 5 segments because
-- we want to self-host and that's as deep as the repo goes.
type PathAPI =
       Get '[HTML] Folder
  :<|> Capture "1a" PathSegment :> Get '[HTML] PathResource
  :<|> Capture "2a" PathSegment :> Capture "2b" PathSegment :> Get '[HTML] PathResource
  :<|> Capture "3a" PathSegment :> Capture "3b" PathSegment :> Capture "3c" PathSegment :> Get '[HTML] PathResource
  :<|> Capture "4a" PathSegment :> Capture "4b" PathSegment :> Capture "4c" PathSegment :> Capture "4d" PathSegment :> Get '[HTML] PathResource
  :<|> Capture "5a" PathSegment :> Capture "5b" PathSegment :> Capture "5c" PathSegment :> Capture "5d" PathSegment :> Capture "5e" PathSegment :> Get '[HTML] PathResource


type PathHandler = EitherT ServantErr IO


joinSegments :: [PathSegment] -> FilePath
joinSegments = joinPath . map textToString


-- | Given a base path and a list of path segments, render eithre the code
-- found on disk at that path or a directory listing for that path.
renderResource :: FilePath -> [PathSegment] -> PathHandler PathResource
renderResource base segments = do
  fileType <- liftIO (getFileType fullPath)
  case fileType of
    Just File -> FileResource <$> renderCode fullPath
    Just Directory -> liftIO (DirResource <$> makeFolder base segments)
    Nothing -> throwError $ err404 { errBody = "no such resource" }

  where
    fullPath = base </> joinSegments segments


-- | Given a path to a file on disk, render the code.
renderCode :: FilePath -> PathHandler HolbornSource
renderCode path = do
  sourceCode <- liftIO $ readFile path  -- TODO: Handle error.
  return $ annotatePythonCode sourceCode


browseCode :: FilePath -> Server PathAPI
browseCode basePath =
  (browseFiles basePath)
  :<|> (\a         -> renderResource basePath [a])
  :<|> (\a b       -> renderResource basePath [a, b])
  :<|> (\a b c     -> renderResource basePath [a, b, c])
  :<|> (\a b c d   -> renderResource basePath [a, b, c, d])
  :<|> (\a b c d e -> renderResource basePath [a, b, c, d, e])


-- XXX: I've fallen victim to one of the classic blunders. Current
-- implementation allows ".." to jailbreak path.
browseFiles :: FilePath -> PathHandler Folder
browseFiles basePath = liftIO $ makeFolder basePath []

-- | Create a server for SyntaxAPI
server :: Text -> FilePath -> Server SyntaxAPI
server pythonSourceCode basePath = return (annotatePythonCode pythonSourceCode) :<|> browseCode basePath


-- TODO: Either use this or kill it with fire.
-- | Given a rendered HTML block of code, return a full HTML with highlighting.
codePage :: ToMarkup a => a -> Html
codePage codeHtml = do
  H.head $ do
    H.title "Example code page"
    H.style ! A.type_ (H.toValue ("text/css" :: Text)) $ toHtml monokai
    -- XXX: Embedding styling here is terrible.
    H.style ! A.type_ (H.toValue ("text/css" :: Text)) $ toHtml (".codehilite { background-color: #333; }" :: Text)
  H.body $ H.div ! A.class_ (H.toValue ("codehilite" :: Text)) $ toHtml codeHtml


instance ToMarkup PathResource where

  toMarkup (FileResource f) = toMarkup f
  toMarkup (DirResource f)  = toMarkup f


instance ToMarkup Folder where

  toMarkup directory = do
    H.h1 (H.toHtml (folderPath directory))
    H.ul (mapM_ (H.li . linkify) (folderChildren directory))
    where
      -- TODO: Use safe links
      linkify segment =
        H.a ! A.href (H.toValue (url segment)) $ H.toHtml segment
      url segment = childURL directory segment
