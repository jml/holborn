module Holborn.API.Auth
       ( getAuthFromToken
       ) where

import BasicPrelude

import Holborn.Errors (APIError(..))
import Holborn.API.Config (AppConf(..))
import Holborn.Auth (AuthToken(..), Permissions(..), userFromToken)
import Control.Monad.Trans.Except (ExceptT, throwE)


-- ExceptT trying to auth the user
getAuthFromToken :: AppConf -> Maybe AuthToken -> ExceptT (APIError a) IO (Int, Permissions)
getAuthFromToken AppConf{conn} token = do
    authToken <- case token of
        Nothing -> throwE MissingAuthToken
        Just x -> return x

    maybeUser <- liftIO $ userFromToken conn authToken
    case maybeUser of
        Just (userId, permissions) -> return (userId, permissions)
        Nothing -> throwE InvalidAuthToken
