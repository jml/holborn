{- | Adapted version of highlighter2 syntax highlighter.  -}

module Holborn.HtmlFormat (format, formatInline) where

import BasicPrelude hiding (div, span)

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString as BS

import Text.Highlighter.Types


format :: Bool -> [Token] -> Html
format ls ts
    | ls =
        H.table ! A.class_ "highlighttable" $ H.tr $ do
            H.td ! A.class_ "linenos" $
                H.div ! A.class_ "linenodiv" $
                    H.pre (lineNos (countLines ts))

            H.td ! A.class_ "code" $
                H.div ! A.class_ "highlight" $
                    H.pre $ highlight ts
    | otherwise =
        H.div ! A.class_ "highlight" $
            H.pre $ highlight ts


formatInline :: [Token] -> Html
formatInline = H.code . highlight


highlightToken :: Token -> Html
highlightToken token@(Token t s) =
  if isReference token
  then H.a ! A.href (H.toValue ("https://google.com/#safe=strict&q=" ++ decodeUtf8 s)) $ baseToken
  else baseToken
  where
    baseToken = H.span ! A.class_ (H.toValue $ shortName t) $ H.toHtml (decodeUtf8 s)


-- | Is this given token a reference to a thing with a definition?
--
-- We use this to decide whether to linkify things or not.
--
-- XXX: Probably should return a richer data type, e.g. Reference | Value
isReference :: Token -> Bool
isReference (Token t _) =
  -- XXX: These aren't empirically verified to be actual references, it's just
  -- a first guess.
  case t of
    Declaration   -> True
    Reserved      -> True
    Type          -> True
    Pseudo        -> True
    Namespace     -> True
    Class         -> True
    Constant      -> True
    Attribute     -> True
    Builtin       -> True
    Decorator     -> True
    Entity        -> True
    Exception     -> True
    Function      -> True
    Identifier    -> True
    Label         -> True
    Property      -> True
    Tag           -> True
    Variable      -> True
    Global        -> True
    Instance      -> True
    Anonymous     -> True
    Arbitrary "Name"      -> True
    Arbitrary "Name" :. _ -> True
    _ -> False


highlight :: [Token] -> Html
highlight = mconcat . map highlightToken


countLines :: [Token] -> Int
countLines = sum . map linesInToken
  where linesInToken (Token _ s) = length $ BS.elemIndices newlineByte s
        newlineByte = 0xA  -- '\n'


lineNos :: Int -> Html
lineNos n = lineNos' 1
  where
    lineNos' c
        | c <= n = do
            H.a ! A.href (H.toValue $ "#L-" ++ show c)
                ! A.name (H.toValue $ "L-" ++ show c) $
              H.toHtml (show c)

            H.toHtml ("\n" :: Text)

            lineNos' (c + 1)
        | otherwise = return ()
