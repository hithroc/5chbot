{-# LANGUAGE OverloadedStrings #-}
import Reddit
import Reddit.Types.Post

import Control.Monad
import Settings
import System.IO
import System.Directory
import System.Environment
import System.Exit
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple hiding (priority)
import System.Posix.Daemonize
import System.Posix.Process
import Bot.Config
import Bot.Core
import Bot.Util

initLogger :: Settings -> IO ()
initLogger (Settings { setLogFile = fp, setLogLevel = lvl, setDaemonMode = mode }) = do
    -- Initialize logger
    let form = simpleLogFormatter "$time [$prio]\t$msg"
    handler <- case fp of
      Nothing -> if mode
        then fileHandler "5chbot.log" DEBUG
        else streamHandler stdout DEBUG
      Just path -> fileHandler path DEBUG
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
    Nothing -> do
      criticalM rootLoggerName "Error: Failed to open config.json!"
      when (setDaemonMode set) $ putStrLn "Failed to start daemon!"
      exitFailure
    Just cfg -> do
      if setDaemonMode set
      then do
        infoM rootLoggerName "Running as a daemon."
        pwd <- getCurrentDirectory
        daemonize $ do
          setCurrentDirectory pwd
          pid <- getProcessID
          infoM rootLoggerName $ "PID: " ++ show pid
          go cfg
       else go cfg

go :: Config -> IO ()
go cfg = do
  res <- runReddit (cfgUsername cfg) (cfgPassword cfg) (redditMain cfg)
  case res of
    Left e -> do
      criticalM rootLoggerName (show e)
      exitFailure
    _ -> return ()
