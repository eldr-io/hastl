{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Init where

import Control.Exception.Safe (
  SomeException (SomeException),
  bracket,
  catch,
  finally,
  onException,
  throwIO,
 )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (typeOf)
import Database.Persist.Postgresql (runSqlPool)
import Network.Wai (Application)
import Say
import System.Environment (lookupEnv)

import Api (app)
import Config (Config (..), Environment (..), makePool, setLogger)
import Data.Pool qualified as Pool
import Katip qualified
import Logger (defaultLogEnv)
import Models (doMigrations)
import Network.Wai.Handler.Warp (run)
import Safe (readMay)

{- | An action that creates a WAI 'Application' together with its resources,
  runs it, and tears it down on exit
-}
runAppDevel :: IO ()
runAppDevel = do
  say "in runAppDevel"
  withConfig $ \config -> do
    say "acquired config"
    cfg <-
      initialize config
        `finally` say "exited: initialize config"
    say "post-initialize"
    let port = configPort config
    say $ "Server running on port " <> Text.pack (show port) <> "..."
    run (configPort config) cfg
      `finally` say "server is closed"

{- | The 'initialize' function accepts the required environment information,
initializes the WAI 'Application' and returns it
-}
initialize :: Config -> IO Application
initialize cfg = do
  say "initialize"
  let logger = setLogger (configEnv cfg)
  say "run migrations"
  bracket
    (say "starting to run migrations")
    (\_ -> say "migrations complete")
    $ \_ -> do
      say "actually running migrations"
      runSqlPool doMigrations (configPool cfg) `catch` \(SomeException e) -> do
        say $
          mconcat
            [ "exception in doMigrations, type: "
            , tshow (typeOf e)
            , ", shown: "
            , tshow e
            ]
        throwIO e
      say "okay all done"
  say "making app"
  pure . logger . app $ cfg

withConfig :: (Config -> IO a) -> IO a
withConfig action = do
  say "acquireConfig"
  port <- lookupSetting "PORT" 8081
  say $ "on port:" <> tshow port
  env <- lookupSetting "ENV" Development
  say $ "on env: " <> tshow env
  bracket defaultLogEnv (\x -> say "closing katip scribes" >> Katip.closeScribes x) $ \logEnv -> do
    say "got log env"
    !pool <- makePool env logEnv `onException` say "exception in makePool"
    say "got pool "
    action
      Config
        { configPool = pool
        , configEnv = env
        , configLogEnv = logEnv
        , configPort = port
        }

-- | Takes care of cleaning up 'Config' resources
shutdownApp :: Config -> IO ()
shutdownApp cfg = do
  _ <- Katip.closeScribes (configLogEnv cfg)
  Pool.destroyAllResources (configPool cfg)
  pure ()

{- | Looks up a setting in the environment, with a provided default, and
'read's that information into the inferred type.
-}
lookupSetting :: (Read a) => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing ->
      return def
    Just str ->
      maybe (handleFailedRead str) return (readMay str)
 where
  handleFailedRead str =
    error $
      mconcat
        [ "Failed to read [["
        , str
        , "]] for environment variable "
        , env
        ]

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
