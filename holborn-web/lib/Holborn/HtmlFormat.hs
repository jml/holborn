{- | Adapted version of highlighter2 syntax highlighter.  -}

module Holborn.HtmlFormat (format, formatInline) where

import BasicPrelude hiding (div, span)

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString as BS

import Text.Highlighter.Types

import Holborn.Types (
  HolbornToken,
  Reference(Reference),
  tokenName,
  tokenReference,
  tokenType,
  )


format :: Bool -> [HolbornToken] -> Html
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


formatInline :: [HolbornToken] -> Html
formatInline = H.code . highlight


highlightToken :: HolbornToken -> Html
highlightToken token =
  -- XXX: We now have two things that can indicate whether something is a
  -- reference: it can have a reference-like token type, or it can have a
  -- Reference associated with it. Not clear what we should do when we have
  -- one and not the other. I guess warn?
  if isReference token
  then H.a ! A.href (H.toValue tokenUrl) $ baseToken
  else baseToken
  where
    baseToken = H.span ! A.class_ (H.toValue . shortName . tokenType $ token) $ H.toHtml contents
    contents = decodeUtf8 (tokenName token)
    tokenUrl = fromMaybe ("https://google.com/#safe=strict&q=" ++ contents) (getUrl token)


-- XXX: I guess theoretically we should be able to get the URL just from the
-- Reference, so the type signature of this should be Reference -> Maybe Text.
-- It's not clear to me how public Reference should be.
getUrl :: HolbornToken -> Maybe Text
getUrl = map (\(Reference r) -> ("#" ++ r)) . tokenReference'


-- XXX: This should probably be the public interface in Holborn.Types.
tokenReference' :: HolbornToken -> Maybe Reference
tokenReference' token =
  case tokenReference token of
    r'@(Reference r)
      | r == "" -> Nothing
      | otherwise -> Just r'


-- | Is this given token a reference to a thing with a definition?
--
-- We use this to decide whether to linkify things or not.
--
-- XXX: Probably should return a richer data type, e.g. Reference | Value
isReference :: HolbornToken -> Bool
isReference token =
  -- XXX: These aren't empirically verified to be actual references, it's just
  -- a first guess.
  case tokenType token of
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


highlight :: [HolbornToken] -> Html
highlight = mconcat . map highlightToken


countLines :: [HolbornToken] -> Int
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
