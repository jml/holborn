{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Holborn.UI.Api
       ( UserAPI
       , userAPI
       , server
       ) where

import BasicPrelude

import Control.Monad.Trans.Either (EitherT)
import Data.Proxy (Proxy)
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html, toHtml)
import Text.Hamlet (shamletFile)
import Data.Text (unpack)


data SignupData = SignupData { username :: Text, email ::Text, password :: Text }

type UserAPI =
    "users" :> "signup" :> Get '[HTML] Html
    :<|> "users" :> "signup" :> ReqBody '[FormUrlEncoded] SignupData :> Post '[HTML] Html


instance FromFormUrlEncoded SignupData where
    fromFormUrlEncoded inputs =
        SignupData <$> f "username" <*> f "email" <*> f "password"
      where f label = case lookup label inputs of
                Nothing -> Left $ "label " ++ unpack label ++ " not found"
                Just v  -> Right v


signupGet :: EitherT ServantErr IO Html
signupGet = do
    return $(shamletFile "./templates/signup.html")


signupPost :: SignupData -> EitherT ServantErr IO Html
signupPost = undefined


server :: Server UserAPI
server = signupGet  :<|> signupPost

userAPI :: Proxy UserAPI
userAPI = Proxy
