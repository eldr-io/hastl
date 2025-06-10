{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Logger (
  adapt,
  createLogEnv,
  defaultLogEnv,
  jsonLogEnv,
  logMsg,
  runKatipT,
  KatipT (..),
  Katip (..),
  LogEnv,
  Severity (..),
  LogFormat(..),
  logJSON,
  logInfoJSON,
  logErrorJSON,
  logDebugJSON,
  severityToText,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (
  Loc,
  LogLevel (..),
  LogSource,
  ToLogStr,
 )
import Control.Monad.Logger qualified as Logger
import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Text.Lazy qualified as TL
import Data.Time (getCurrentTime)
import System.IO (Handle)
import Katip (
  ColorStrategy (ColorIfTerminal),
  Item (..),
  Katip (..),
  KatipT (..),
  LogEnv,
  LogStr,
  Namespace (Namespace, unNamespace),
  Scribe (..),
  Severity (..),
  Verbosity (V2),
  defaultScribeSettings,
  initLogEnv,
  logMsg,
  logStr,
  mkHandleScribe,
  permitItem,
  registerScribe,
  runKatipT,
 )
import System.IO qualified as IO
import System.Log.FastLogger qualified as FastLogger

-- | Log output format configuration
data LogFormat 
  = TextFormat    -- ^ Human-readable text format (for development)
  | JSONFormat    -- ^ JSON format (for production)
  deriving (Show, Eq)

-- | Create a log environment with the specified format
createLogEnv :: LogFormat -> IO LogEnv
createLogEnv TextFormat = textLogEnv
createLogEnv JSONFormat = jsonLogEnv

-- | Default log environment (backwards compatibility)
defaultLogEnv :: IO LogEnv
defaultLogEnv = textLogEnv

-- | Text-based logging environment (original behavior)
textLogEnv :: IO LogEnv
textLogEnv = do
  handleScribe <- Katip.mkHandleScribe ColorIfTerminal IO.stdout (permitItem DebugS) V2
  env <- initLogEnv "hastl" "production"
  registerScribe "stdout" handleScribe defaultScribeSettings env

-- | JSON-based logging environment for production
jsonLogEnv :: IO LogEnv
jsonLogEnv = do
  jsonScribe <- mkJsonScribe IO.stdout (\sev -> return $ sev >= DebugS) V2
  env <- initLogEnv "hastl" "production"
  registerScribe "stdout" jsonScribe defaultScribeSettings env

-- | Create a JSON scribe that formats all log messages as JSON
mkJsonScribe :: Handle -> (Severity -> IO Bool) -> Verbosity -> IO Scribe
mkJsonScribe _handle permitF _verbosity = do
  lockingAction <- FastLogger.newStdoutLoggerSet FastLogger.defaultBufSize >>= \loggerSet ->
    return $ \logStrData -> do
      FastLogger.pushLogStr loggerSet logStrData
      FastLogger.flushLogStr loggerSet
  
  return $ Scribe
    { liPush = \item -> do
        let sev = _itemSeverity item
        permitted <- permitF sev
        if permitted
          then do
            jsonLog <- itemToJSON item
            let jsonText = TLE.decodeUtf8 $ A.encode jsonLog
            lockingAction (FastLogger.toLogStr $ jsonText <> "\n")
          else return ()
    , scribeFinalizer = return ()
    , scribePermitItem = \item -> permitF (_itemSeverity item)
    }

-- | Convert a Katip Item to JSON object
itemToJSON :: Item a -> IO A.Value
itemToJSON item = do
  timestamp <- getCurrentTime
  let nsText = T.intercalate "." $ unNamespace $ _itemNamespace item
  let msgText = T.take 1000 $ T.pack $ show $ _itemMessage item
  return $ A.object
    [ "timestamp" .= timestamp
    , "level" .= severityToText (_itemSeverity item)
    , "namespace" .= nsText
    , "message" .= (T.strip $ T.replace "LogStr {unLogStr = \"" "" $ T.replace "\"}" "" msgText)
    , "thread" .= (T.strip $ T.replace "ThreadIdText {getThreadIdText = \"" "" $ T.replace "\"}" "" $ T.pack $ show $ _itemThread item)
    , "host" .= _itemHost item
    , "process" .= show (_itemProcess item)
    , "app" .= (T.intercalate "." $ unNamespace $ _itemApp item)
    , "env" .= _itemEnv item
    ]

-- | Convert Katip severity to text representation for JSON
severityToText :: Severity -> Text
severityToText DebugS = "debug"
severityToText InfoS = "info"
severityToText NoticeS = "notice"
severityToText WarningS = "warning"
severityToText ErrorS = "error"
severityToText CriticalS = "critical"
severityToText AlertS = "alert"
severityToText EmergencyS = "emergency"

-- | Convert MonadLogger levels to Katip severity
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

-- | Helper function to create structured JSON log messages
-- This can be used directly in application code for structured logging
logJSON :: (Katip m, MonadIO m, A.ToJSON payload) => Namespace -> Severity -> Text -> payload -> m ()
logJSON ns sev msg payload = do
  -- Create a structured log message that will be formatted properly by the JSON scribe
  let structuredMsg = msg <> " | " <> (T.take 500 $ TL.toStrict $ TLE.decodeUtf8 $ A.encode payload)
  logMsg ns sev (Katip.logStr structuredMsg)

-- | Simplified structured logging for common use cases
logInfoJSON :: (Katip m, MonadIO m, A.ToJSON payload) => Text -> payload -> m ()
logInfoJSON msg payload = logJSON mempty InfoS msg payload

logErrorJSON :: (Katip m, MonadIO m, A.ToJSON payload) => Text -> payload -> m ()
logErrorJSON msg payload = logJSON mempty ErrorS msg payload

logDebugJSON :: (Katip m, MonadIO m, A.ToJSON payload) => Text -> payload -> m ()
logDebugJSON msg payload = logJSON mempty DebugS msg payload