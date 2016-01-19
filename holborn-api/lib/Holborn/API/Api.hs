{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
module Holborn.API.Api
       ( UserAPI
       , userAPI
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
import qualified Holborn.JSON.Response as U
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


type UserAPI =
    Get '[HTML] Html
    :<|> "v1" :> "users" :> Capture "username" Username :> Get '[JSON] (U.Result U.ListUsersRow Text)
    :<|> "users" :> "signup" :> ReqBody '[JSON] SignupData :> Post '[JSON] (U.Result SignupOk SignupError)


landing :: EitherT ServantErr IO Html
landing = return $(shamletFile "./templates/landing.html")


getUser :: AppConf -> Username -> EitherT ServantErr IO (U.Result U.ListUsersRow Text)
getUser (AppConf conn _) username = do
    r <- liftIO (runExceptT (U.getUser conn username))
    case r of
        Left (UserNotFound u) -> left err404
        Right row -> return (U.OK row)


signupPost :: AppConf -> SignupData -> EitherT ServantErr IO (U.Result SignupOk SignupError)
signupPost (AppConf conn jwtSecret) SignupData{..} = do
    pwd <- liftIO (newPassword password)
    result <- liftIO (runExceptT (U.signup conn (newUsername username) (newEmail email) pwd))
    case result of
        Left (UserAlreadyExists u) -> return $ U.Error (EUserExists "user already exists")
        Left err@(UnexpectedConstraintViolation x) -> do
            liftIO $ print ("ERROR", err)
            left err400
        Left _ -> left err400
        Right _ -> do
            let jwt = JWT.def { JWT.sub = (JWT.stringOrURI username) }
            let encodedJwt = JWT.encodeSigned JWT.HS256 (JWT.secret jwtSecret) jwt
            return (U.OK (AuthJwt encodedJwt))


server :: AppConf -> Server UserAPI
server conf =
    landing
    :<|> (getUser conf)
    :<|> (signupPost conf)


userAPI :: Proxy UserAPI
userAPI = Proxy
