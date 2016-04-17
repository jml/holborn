{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Holborn.API.Api
       ( API
       , server
       ) where

import BasicPrelude

import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant

import qualified Web.JWT as JWT
import Database.PostgreSQL.Simple (Only (..), query)
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Holborn.API.Templates (landingPage)
import Holborn.API.Types (ApiError(..), newEmail, newPassword, newUsername, checkPassword, Password, Username, AppConf(..))
import Holborn.JSON.User (SigninData(..), SigninOK(..))
import qualified Holborn.API.User as U
import Holborn.Auth (webPermissions, createAuthToken)
import Network.Wai (Application, responseLBS)
import Network.HTTP.Types.Status (status200)
import Text.Blaze.Renderer.Utf8 (renderMarkup)


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

instance FromHttpApiData Username where
    parseUrlPiece x = pure (newUsername x)


type API =
    "users" :> "signup" :> ReqBody '[JSON] SignupData :> Post '[JSON] (Either SignupError SignupOk)
    :<|> "v1" :> "signin" :> ReqBody '[JSON] SigninData :> Post '[JSON] SigninOK
    :<|> Raw
    -- Special POST for signing up / in etc

landing :: AppConf -> Application
landing AppConf{staticBaseUrl, baseUrl} = \_request respond ->
  respond (responseLBS status200 [] (renderMarkup (landingPage baseUrl staticBaseUrl)))


signupPost :: AppConf -> SignupData -> ExceptT ServantErr IO (Either SignupError SignupOk)
signupPost AppConf{conn, jwtSecret} SignupData{..} = do
    pwd <- liftIO (newPassword _password)
    result <- liftIO (runExceptT (U.signup conn (newUsername username) (newEmail email) pwd))
    case result of
        Left (UserAlreadyExists _) -> return $ Left (EUserExists "user already exists")
        Left (UnexpectedConstraintViolation _) -> do
            throwE err400
        Left _ -> throwE err400
        Right _ -> do
            let jwt = JWT.def { JWT.sub = (JWT.stringOrURI username) }
            let encodedJwt = JWT.encodeSigned JWT.HS256 (JWT.secret jwtSecret) jwt
            return (Right (AuthJwt encodedJwt))


-- | Signin creates a oauth token for the "Web Login" realm if the
-- password verified OK (checkPassword). The token is stored as a
-- cookie in the client.
signin :: AppConf -> SigninData -> ExceptT ServantErr IO SigninOK
signin AppConf{conn} SigninData{..} = do
    r <- liftIO $ query conn [sql|
                   select id, password
                   from "user"
                   where username = ?
               |] (Only _SigninData_username)

    ownerId <- case r of
      [(id_ :: Int, pwd :: Password)]
        | checkPassword pwd (encodeUtf8 _SigninData_password) -> return id_
        | otherwise -> throwE err400  -- Wrong password
      _ -> throwE err400 -- TODO encode errors

    token <- liftIO createAuthToken

    [Only (_ :: Int)] <- liftIO $ query conn [sql|
            insert into "oauth_token" (description, owner_id, token, permissions)
            values (?, ?, ?, ?) returning id
            |] ("Web Login" :: Text, ownerId, token, webPermissions)

    return (SigninOK token)


server :: AppConf -> Server API
server conf =
    signupPost conf
    :<|> signin conf
    :<|> landing conf
