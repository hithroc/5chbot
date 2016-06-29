{-# LANGUAGE OverloadedStrings #-}
import Reddit
import Reddit.Types.Post

import Bot.Config
import Bot.Core
import Bot.Util
import Settings
import System.IO
import System.Environment
import System.Exit
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple hiding (priority)

initLogger :: Settings -> IO ()
initLogger (Settings { setLogFile = fp, setLogLevel = lvl }) = do
    -- Initialize logger
    let form = simpleLogFormatter "$time [$prio]\t$msg"
    handler <- case fp of
      Nothing -> streamHandler stdout DEBUG
      Just fp' -> fileHandler fp' DEBUG
    updateGlobalLogger rootLoggerName $ setHandlers [setFormatter handler form]
    updateGlobalLogger rootLoggerName $ setLevel lvl
    infoM rootLoggerName showVersion

main :: IO ()
main = do
  argv <- getArgs
  (opts, _) <- compilerOpts argv
  set <- getSettings opts
  initLogger set
  mcfg <- loadConfig "config.json"
  case mcfg of
    Nothing -> criticalM rootLoggerName "Error: Failed to open config.json!"
    Just cfg -> do
      res <- runReddit (cfgUsername cfg) (cfgPassword cfg) (redditMain cfg)
      case res of
        Left e -> do
          criticalM rootLoggerName (show e)
          exitFailure
        _ -> return ()
