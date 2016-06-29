module Settings where

import System.Console.GetOpt
import System.Environment
import System.Exit
import Control.Monad
import Paths_5chbot (version)
import qualified Data.Version as V
import System.Log
import Bot.Util

data Settings
  = Settings
  { setDaemonMode :: Bool
  , setLogFile :: Maybe FilePath
  , setLogLevel :: Priority
  }
  deriving Show

defaultSettings :: Settings
defaultSettings = Settings
  { setDaemonMode = False
  , setLogFile = Nothing
  , setLogLevel = INFO
  }

data Flag
  = DaemonMode
  | LogFile FilePath
  | PrintVersion
  | PrintHelp
  deriving Show

options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"]     (NoArg PrintHelp)       "Print this message"
  , Option ['D'] ["daemon"]   (NoArg DaemonMode)      "Run the program in daemon mode"
  , Option []    ["version"]  (NoArg PrintVersion)    "Print program's version"
  , Option []    ["log-file"] (ReqArg LogFile "FILE") "Log file. Default is stdout"
  ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = case getOpt Permute options argv of
  (o,n,[]) -> return (o,n)
  (_,_,err) -> do
    name <- getProgName
    let header = "Usage: " ++ name ++ "[OPTIONS]"
    putStrLn $ concat err
    printHelp
    exitFailure

printHelp :: IO ()
printHelp = do
  name <- getProgName
  let header = "Usage: " ++ name ++ " [OPTIONS]"
  putStrLn $ usageInfo header options

printVersion :: IO ()
printVersion = putStrLn showVersion


applyOpt :: Flag -> Settings -> IO Settings
applyOpt DaemonMode s = return $ s { setDaemonMode = True }
applyOpt (LogFile p) s = return $ s { setLogFile = Just p }
applyOpt PrintVersion _ = do
  printVersion
  exitSuccess
applyOpt PrintHelp _ = do
  printHelp
  exitSuccess

getSettings :: [Flag] -> IO Settings
getSettings = foldM (flip applyOpt) defaultSettings
