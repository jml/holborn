{- | Adapted version of highlighter2 syntax highlighter.  -}

module Holborn.HtmlFormat (format, formatInline) where

import BasicPrelude hiding (div, span)

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString as BS

import Text.Highlighter.Types

import Holborn.Types (
  Annotation(..),
  HolbornToken,
  tokenAnnotation,
  tokenName,
  tokenType,
  )


format :: (H.ToValue a, Show a) => Bool -> [HolbornToken a] -> Html
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


formatInline :: (H.ToValue a, Show a) => [HolbornToken a] -> Html
formatInline = H.code . highlight


highlightToken :: (H.ToValue a, Show a) => HolbornToken a -> Html
highlightToken token =
  case tokenAnnotation token of
    Nothing -> baseToken
    Just (Binding i) -> H.a ! A.href (H.toValue findReferencesUrl) ! A.id (H.toValue i) $ baseToken
    Just (Reference i) -> H.a ! A.href (H.toValue (bindingUrl i)) $ baseToken
    -- XXX: What do we actually want to do for unresolved references? Some sort of styling?
    Just UnresolvedReference -> H.span ! A.class_ (H.toValue ("unresolved" :: Text)) $ baseToken
  where
    baseToken = H.span ! A.class_ (H.toValue . shortName . tokenType $ token) $ H.toHtml contents
    contents = decodeUtf8 (tokenName token)
    findReferencesUrl = "https://google.com/?q=" ++ decodeUtf8 (tokenName token)
    bindingUrl i = "#" ++ show i


highlight :: (H.ToValue a, Show a) => [HolbornToken a] -> Html
highlight = mconcat . map highlightToken


countLines :: [HolbornToken a] -> Int
countLines = sum . map linesInToken
  where linesInToken = length . BS.elemIndices newlineByte . tokenName
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
