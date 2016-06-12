{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | Adapted version of highlighter2 syntax highlighter.  -}

module Holborn.Repo.HtmlFormatTokens () where

import HolbornPrelude hiding (div, span)

import Text.Blaze (ToMarkup(..))
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString as BS

import Holborn.Syntax (
  Annotation(..),
  HolbornSource,
  HolbornToken,
  getTokens,
  tokenAnnotation,
  tokenName,
  tokenShortName,
  )


data LineNumbersOption = ShowLineNumbers | HideLineNumbers


format :: (H.ToValue a, Show a) => LineNumbersOption -> [HolbornToken a] -> Html
format ls ts =
  case ls of
    ShowLineNumbers ->
      H.table ! A.class_ "highlighttable" $ H.tr $ do
        H.td ! A.class_ "linenos" $
          H.div ! A.class_ "linenodiv" $ H.pre (lineNos (countLines ts))

        H.td ! A.class_ "code" $
          H.div ! A.class_ "highlight" $
          H.pre $ highlight ts
    HideLineNumbers ->
        H.div ! A.class_ "highlight" $ H.pre $ highlight ts
  where
    highlight = mconcat . map toMarkup


instance (H.ToValue a, Show a) => ToMarkup (HolbornToken a) where
  toMarkup token =
   case tokenAnnotation token of
     Nothing -> baseToken
     Just (Binding i) -> H.a ! A.href (H.toValue findReferencesUrl) ! A.id (H.toValue i) $ baseToken
     Just (Reference i) -> H.a ! A.href (H.toValue (bindingUrl i)) $ baseToken
     -- XXX: What do we actually want to do for unresolved references? Some sort of styling?
     Just UnresolvedReference -> H.span ! A.class_ (H.toValue ("unresolved" :: Text)) $ baseToken
   where
     baseToken = H.span ! A.class_ (tokenShortName token) $ H.toHtml contents
     contents = decodeUtf8 (tokenName token)
     findReferencesUrl = "https://google.com/?q=" ++ decodeUtf8 (tokenName token)
     bindingUrl i = "#" ++ show i


instance ToMarkup HolbornSource where

  toMarkup source = format HideLineNumbers (getTokens source)


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
