{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Holborn.API.Api
       ( API
       , server
       ) where

import BasicPrelude

import Control.Error (runExceptT)
import Control.Monad.Trans.Either (EitherT, left)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (unpack)
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html)
import Text.Hamlet (shamletFile)
import qualified Web.JWT as JWT
import Database.PostgreSQL.Simple (Only (..), execute, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Holborn.API.Types (ApiError(..), newEmail, newPassword, newUsername, checkPassword, Password, Username, AppConf(..))
import Holborn.JSON.User (SigninData(..), SigninOK(..))
import qualified Holborn.API.User as U
import qualified Holborn.JSON.User as U
import Holborn.Auth (webPermissions, createAuthToken)

data SignupData = SignupData
    { username :: Text
    , email ::Text
    , _password :: Text
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
    -- Special POST for signing up / in etc
    :<|> "users" :> "signup" :> ReqBody '[JSON] SignupData :> Post '[JSON] (Either SignupError SignupOk)
    :<|> "v1" :> "signin" :> ReqBody '[JSON] SigninData :> Post '[JSON] SigninOK


landing :: EitherT ServantErr IO Html
landing = return $(shamletFile "./templates/landing.html")


signupPost :: AppConf -> SignupData -> EitherT ServantErr IO (Either SignupError SignupOk)
signupPost (AppConf conn jwtSecret) SignupData{..} = do
    pwd <- liftIO (newPassword _password)
    result <- liftIO (runExceptT (U.signup conn (newUsername username) (newEmail email) pwd))
    case result of
        Left (UserAlreadyExists _) -> return $ Left (EUserExists "user already exists")
        Left err@(UnexpectedConstraintViolation _) -> do
            left err400
        Left _ -> left err400
        Right _ -> do
            let jwt = JWT.def { JWT.sub = (JWT.stringOrURI username) }
            let encodedJwt = JWT.encodeSigned JWT.HS256 (JWT.secret jwtSecret) jwt
            return (Right (AuthJwt encodedJwt))


-- | Signin creates a oauth token for the "Web Login" realm if the
-- password verified OK (checkPassword). The token is stored as a
-- cookie in the client.
signin :: AppConf -> SigninData -> EitherT ServantErr IO SigninOK
signin (AppConf conn _) SigninData{..} = do
    pwd <- liftIO (newPassword _SigninData_password)
    r <- liftIO $ query conn [sql|
                   select id, password
                   from "user"
                   where username = ?
               |] (Only _SigninData_username)

    (ownerId, password) <- case r of
      [(id_ :: Int, pwd :: Password)] -> return (id_, pwd)
      _ -> left err400 -- TODO encode errors

    unless (checkPassword password (encodeUtf8 _SigninData_password)) (left err400)

    token <- liftIO createAuthToken

    [Only (_ :: Int)] <- liftIO $ query conn [sql|
            insert into "oauth_token" (description, owner_id, token, permissions)
            values (?, ?, ?, ?) returning id
            |] ("Web Login" :: Text, ownerId, token, webPermissions)

    return (SigninOK token)


server :: AppConf -> Server API
server conf =
    landing
    :<|> (signupPost conf)
    :<|> (signin conf)
