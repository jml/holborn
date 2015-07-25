{- | Adapted version of highlighter2 syntax highlighter.  -}

module Holborn.HtmlFormat (format, formatInline) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Prelude hiding (div, span)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (span)
import qualified Data.ByteString as BS

import Text.Highlighter.Types


format :: Bool -> [Token] -> Html
format ls ts
    | ls =
        table ! class_ "highlighttable" $ tr $ do
            td ! class_ "linenos" $
                div ! class_ "linenodiv" $
                    pre (lineNos (countLines ts))

            td ! class_ "code" $
                div ! class_ "highlight" $
                    pre $ highlight ts
    | otherwise =
        div ! class_ "highlight" $
            pre $ highlight ts

formatInline :: [Token] -> Html
formatInline = code . highlight

highlight :: [Token] -> Html
highlight [] = return ()
highlight (Token t s:ts) = do
    span ! class_ (toValue $ shortName t) $ toHtml (decodeUtf8 s)
    highlight ts

countLines :: [Token] -> Int
countLines [] = 0
countLines (Token _ s:ts) =
    length (BS.elemIndices (toEnum . fromEnum $ '\n') s) + countLines ts

lineNos :: Int -> Html
lineNos n = lineNos' 1
  where
    lineNos' c
        | c <= n = do
            a ! href (toValue $ "#L-" ++ show c)
              ! name (toValue $ "L-" ++ show c) $
                toHtml (show c)

            toHtml ("\n" :: Text)

            lineNos' (c + 1)
        | otherwise = return ()
