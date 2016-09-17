module Holborn.Repo.Search where

import HolbornPrelude
import Holborn.Repo.GitLayer (Repository, repoPath)
import qualified Text.Megaparsec.Text as P
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Lexer as PL
import System.Process (readProcess)
import qualified Data.Set as Set
import Data.Foldable (fold)
import Data.Text (unpack)

data IncludeExclude = Include | Exclude deriving (Show, Eq)

data Term =
    Plain Text -- Can be a regex maybe?
  | Language Text
  | FileName Text -- regex?
  | UnknownTermType Text -- will be ignored during search
  deriving (Show, Eq)

type Search = [(IncludeExclude, Term)]


data Match = Match
  { path :: String
  , line :: Integer
  , contents :: String
  }
  deriving (Show, Eq, Ord)

parseIncludeExclude :: P.Parser IncludeExclude
parseIncludeExclude = (P.char '-' >> pure Exclude) <|> pure Include

parseQuoted :: P.Parser String
parseQuoted = P.char '"' >> P.manyTill P.anyChar (P.char '"')

parseTerm :: P.Parser (IncludeExclude, Term)
parseTerm = do
  ie <- parseIncludeExclude
  value <- (P.try parseQuoted) <|> (P.some (P.alphaNumChar <|> P.symbolChar <|> P.punctuationChar))
  pure (ie, Plain (fromString value))


-- TODO quickcheck that inputs produce an output, and that runtime is
-- bounded by O(N). E.g.:
-- λ  quickCheck (\s -> let Just x = parseSearch (fromString s) in True)
parseSearch :: Text -> Maybe Search
parseSearch s = P.parseMaybe (P.sepBy parseTerm P.space) s


-- Stupid implementation of search, this implementation should be
-- replaced by a smarter one that uses indices and a non-pcre regex
-- engine to avoid non-linear runtimes. Also avoid `String`
-- (e.g. using System.Process.ByteString)
runBasicSearch :: Repository -> Search -> IO [Match]
runBasicSearch repo search = do
  e <- exclude
  i <- include
  return (Set.toList (i `Set.difference` e))
  where
    exclude = fmap (Set.fromList . (fromMaybe []) . fold) (mapM excludeRun search)
    excludeRun (Exclude, x) = runOneGrep repo x
    excludeRun _ = pure Nothing

    include = fmap (Set.fromList . (fromMaybe []) . fold) (mapM includeRun search)
    includeRun (Include, x) = runOneGrep repo x
    includeRun _ = pure Nothing


runOneGrep :: Repository -> Term -> IO (Maybe [Match])
runOneGrep repo term =
  -- Ignoring in readProcess a bad idea but we do get git complaining
  -- on stderr if there is an actual issue.
  case term of
    Plain text -> parseGitGrep <$> fmap fromString (gitGrep text `catch` \(_ :: SomeException) -> (pure "" :: IO String))
    -- TODO implement remaining patterns
    FileName name -> undefined
    _ -> pure Nothing
  where
    gitGrep text = readProcess "git" ["--git-dir", repoPath repo, "grep", "-n", "--cached", "--heading", unpack text] ""

-- output of git grep looks like this for a file (a:b) with one line "abc"
-- $ gg --heading -n a
-- a:b
-- 1:abc
parseGitGrep :: Text -> Maybe [Match]
parseGitGrep s =
  let p :: P.Parser [Match] = (P.many (Match <$> (P.someTill P.anyChar P.eol) <*> PL.decimal <*> ((P.string ":") >> P.someTill P.anyChar P.eol)))
  in P.parseMaybe p s
