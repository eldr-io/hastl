{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Models where

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH (
  mkMigrate,
  mkPersist,
  persistLowerCase,
  share,
  sqlSettings,
 )

import Config (Config, configPool)
import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Say (say)

share
  [ mkPersist sqlSettings
  , mkMigrate "migrateAll"
  ]
  [persistLowerCase|
User json
    name Text
    email Text
    createdAt UTCTime default=now()
    UniqueEmail email
    deriving Show Eq
|]

doMigrations :: SqlPersistT IO ()
doMigrations = do
  liftIO $ say "in doMigrations, running?"
  runMigration migrateAll
  liftIO $ say "already run"

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks configPool
  liftIO $ runSqlPool query pool

tryRunDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO a -> m (Either SomeException a)
tryRunDb query = do
  pool <- asks configPool
  liftIO $ try $ runSqlPool query pool
