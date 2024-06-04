{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Control.Monad.Except (MonadIO)
import Control.Monad.Logger (logDebugNS)
import Data.Int (Int64)
import Database.Persist.Postgresql (
  Entity (..),
  fromSqlKey,
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

import Config (AppT (..))
import Data.Text (Text)
import Lucid (Html, ToHtml (toHtml), class_, div_, p_)
import Servant.API.ContentTypes.Lucid ( HTML )
import Models (User (User), runDb, userEmail, userName)
import Models qualified as Md

type UserAPI =
  "users" :> Get '[JSON] [Entity User]
    :<|> "users" :> Capture "name" Text :> Get '[JSON] (Entity User)
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
    :<|> "usersTable" :> Get '[HTML] (Html ())

userApi :: Proxy UserAPI
userApi = Proxy

-- | The server that runs the UserAPI
userServer :: (MonadIO m) => ServerT UserAPI (AppT m)
userServer = allUsers :<|> singleUser :<|> createUser :<|> allUsersTbl

-- | Returns all users in the database.
allUsers :: (MonadIO m) => AppT m [Entity User]
allUsers = do
  logDebugNS "web" "allUsers"
  runDb (selectList [] [])

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
createUser :: (MonadIO m) => User -> AppT m Int64
createUser p = do
  logDebugNS "web" "creating a user"
  newUser <- runDb (insert (User (userName p) (userEmail p)))
  return $ fromSqlKey newUser

allUsersTbl :: (MonadIO m) => AppT m (Html ())
allUsersTbl = do
  users :: [Entity User] <- runDb (selectList [] [])
  return $
    div_ [class_ ""] (do (mapM_ f users))
  where f (Entity _ u) = p_ (toHtml (userName u))

