{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Control.Monad.Except (MonadIO)
import Control.Monad.Logger (logDebugNS)
import Database.Persist.Postgresql (
  Entity (..),
  insert,
  selectFirst,
  selectList,
  (==.),
 )
import Servant (
  Capture,
  Get,
  HasServer (ServerT),
  JSON,
  Post,
  Proxy (..),
  ReqBody,
  err404,
  throwError,
  type (:<|>) (..),
  type (:>),
 )

import Api.Templates.Helpers.Htmx (hxTarget_)
import Api.Templates.User.User (renderUser, renderUsersComponent)
import Config (AppT (..))
import Data.Text (Text)
import Lucid (Html, div_)
import Models (User (User), runDb, userEmail, userName)
import Models qualified as Md
import Servant.API.ContentTypes.Lucid (HTML)

type UserAPI =
  "users" :> Get '[HTML] (Html ())
    :<|> "users" :> Capture "name" Text :> Get '[JSON] (Entity User)
    :<|> "users" :> ReqBody '[JSON] User :> Post '[HTML] (Html ())

userApi :: Proxy UserAPI
userApi = Proxy

-- | The server that runs the UserAPI
userServer :: (MonadIO m) => ServerT UserAPI (AppT m)
userServer = allUsers :<|> singleUser :<|> createUser

-- | Returns all users in the database.
allUsers :: (MonadIO m) => AppT m (Html ())
allUsers = do
  logDebugNS "web" "allUsers"
  users :: [Entity User] <- runDb (selectList [] [])
  return $ renderUsersComponent users

-- | Returns a user by name or throws a 404 error.
singleUser :: (MonadIO m) => Text -> AppT m (Entity User)
singleUser str = do
  logDebugNS "web" "singleUser"
  maybeUser <- runDb (selectFirst [Md.UserName ==. str] [])
  case maybeUser of
    Nothing ->
      throwError err404
    Just person ->
      return person

-- | Creates a user in the database.
createUser :: (MonadIO m) => User -> AppT m (Html ())
createUser u = do
  logDebugNS "web" "creating a user"
  newUser <- runDb (insert (User (userName u) (userEmail u)))
  maybeUser <- runDb (selectFirst [Md.UserId ==. newUser] [])
  case maybeUser of
    Nothing -> return $ div_ [hxTarget_ "#errors"] "Failed to create user"
    Just user -> do
      return $ renderUser user
