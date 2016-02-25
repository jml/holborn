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
import Data.Proxy (Proxy)
import Data.Text (unpack)
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html, toHtml)
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
    -- normal user api
    :<|> "v1" :> "users" :> Capture "username" Username :> Get '[JSON] U.ListUsersRow

    -- Special POST for signing up / in etc
    :<|> "users" :> "signup" :> ReqBody '[JSON] SignupData :> Post '[JSON] (Either SignupError SignupOk)
    :<|> "v1" :> "signin" :> ReqBody '[JSON] SigninData :> Post '[JSON] SigninOK


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
    pwd <- liftIO (newPassword _password)
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


signin :: AppConf -> SigninData -> EitherT ServantErr IO SigninOK
signin (AppConf conn jwtSecret) SigninData{..} = do
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

    [Only (tokenId :: Int)] <- liftIO $ query conn [sql|
            insert into "oauth_token" (description, owner_id, token, permissions)
            values (?, ?, ?, ?) returning id
            |] ("Web Login" :: Text, ownerId, token, webPermissions)

    return (SigninOK token)


server :: AppConf -> Server API
server conf =
    landing
    :<|> (getUser conf)
    :<|> (signupPost conf)
    :<|> (signin conf)
