module Holborn.Config where

import Prelude ((++))

foreign import configuration :: { baseUrl :: String }

makeUrl :: String -> String
makeUrl s = configuration.baseUrl ++ s
