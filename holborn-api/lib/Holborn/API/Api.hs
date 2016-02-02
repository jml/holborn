{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
module Holborn.API.Api
       ( API
       , server
       ) where

import BasicPrelude

import Control.Error (runExceptT)
import Control.Monad.Trans.Either (EitherT, left)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Proxy (Proxy)
import Data.Text (unpack)
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html, toHtml)
import Text.Hamlet (shamletFile)
import qualified Holborn.JSON.User as U
import qualified Web.JWT as JWT

import Holborn.API.Types (ApiError(..), newEmail, newPassword, newUsername, Username, AppConf(..))
import qualified Holborn.API.User as U

data SignupData = SignupData
    { username :: Text
    , email ::Text
    , password :: Text
    } deriving Generic
data SignupError = EUserExists Text | EOtherError deriving Generic
data SignupOk = AuthJwt Text deriving Generic

instance FromJSON SignupData
instance ToJSON SignupError
instance ToJSON SignupOk


instance FromText Username where
    fromText x = Just (newUsername x)


type API =
    Get '[HTML] Html
    -- normal user api
    :<|> "v1" :> "users" :> Capture "username" Username :> Get '[JSON] U.ListUsersRow

    -- Special POST for signing up:
    :<|> "users" :> "signup" :> ReqBody '[JSON] SignupData :> Post '[JSON] (Either SignupError SignupOk)


landing :: EitherT ServantErr IO Html
landing = return $(shamletFile "./templates/landing.html")


getUser :: AppConf -> Username -> EitherT ServantErr IO U.ListUsersRow
getUser (AppConf conn _) username = do
    r <- liftIO (runExceptT (U.getUser conn username))
    case r of
        Left (UserNotFound u) -> left err404
        Right row -> return row


signupPost :: AppConf -> SignupData -> EitherT ServantErr IO (Either SignupError SignupOk)
signupPost (AppConf conn jwtSecret) SignupData{..} = do
    pwd <- liftIO (newPassword password)
    result <- liftIO (runExceptT (U.signup conn (newUsername username) (newEmail email) pwd))
    case result of
        Left (UserAlreadyExists u) -> return $ Left (EUserExists "user already exists")
        Left err@(UnexpectedConstraintViolation x) -> do
            liftIO $ print ("ERROR", err)
            left err400
        Left _ -> left err400
        Right _ -> do
            let jwt = JWT.def { JWT.sub = (JWT.stringOrURI username) }
            let encodedJwt = JWT.encodeSigned JWT.HS256 (JWT.secret jwtSecret) jwt
            return (Right (AuthJwt encodedJwt))


server :: AppConf -> Server API
server conf =
    landing
    :<|> (getUser conf)
    :<|> (signupPost conf)
