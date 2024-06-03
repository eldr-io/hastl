{-# LANGUAGE OverloadedStrings #-}

module Logger (
  adapt,
  defaultLogEnv,
  logMsg,
  runKatipT,
  KatipT (..),
  Katip (..),
  LogEnv,
  Severity (..),
) where

import Control.Monad.Logger
    ( ToLogStr, LogLevel(..), LogSource, Loc )
import qualified Control.Monad.Logger as Logger
import Katip
    ( defaultScribeSettings,
      initLogEnv,
      logMsg,
      logStr,
      permitItem,
      registerScribe,
      runKatipT,
      mkHandleScribe,
      Katip(..),
      KatipT(..),
      LogEnv,
      LogStr,
      Namespace(Namespace),
      Severity(..),
      Verbosity(V2),
      ColorStrategy(ColorIfTerminal) )
import qualified System.IO as IO
import qualified System.Log.FastLogger as FastLogger

defaultLogEnv :: IO LogEnv
defaultLogEnv = do
  handleScribe <- Katip.mkHandleScribe ColorIfTerminal IO.stdout (permitItem DebugS) V2
  env <- initLogEnv "hastl" "production"
  registerScribe "stdout" handleScribe defaultScribeSettings env

fromLevel :: LogLevel -> Severity
fromLevel LevelDebug = DebugS
fromLevel LevelInfo = InfoS
fromLevel LevelWarn = WarningS
fromLevel LevelError = ErrorS
fromLevel (LevelOther _) = NoticeS

{- | Transforms Katip logMsg into monadLoggerLog to be used inside
MonadLogger monad
-}
adapt ::
  (ToLogStr msg, Applicative m, Katip m) =>
  (Namespace -> Severity -> Katip.LogStr -> m ()) ->
  Loc ->
  LogSource ->
  LogLevel ->
  msg ->
  m ()
adapt f _ src lvl msg =
  f ns (fromLevel lvl) $ logStr' msg
 where
  ns = Namespace [src]
  -- not sure how fast this is going to be
  logStr' = Katip.logStr . FastLogger.fromLogStr . Logger.toLogStr
