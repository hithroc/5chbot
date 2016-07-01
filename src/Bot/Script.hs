module Bot.Script where

import System.Log.Logger
import System.Process
import System.Directory
import System.Exit
import qualified Data.Text as Text
import qualified Control.Exception as E
import Bot.Config

wrapperScript :: Config -> [String] -> IO (Either String String)
wrapperScript cfg args = do
  let
    scriptPath :: FilePath
    scriptPath = "/scripts/spreadsheet_wrapper.py"
    handler :: E.SomeException -> IO (ExitCode, String, String)
    handler e = return (ExitFailure 1, "", show e)
    args' :: [String]
    args' = (Text.unpack $ cfgMaillistId cfg):args
  path <- (++scriptPath) <$> getCurrentDirectory
  (c, out, err) <- E.handle handler $ readCreateProcessWithExitCode (proc path args') ""
  case c of
    ExitFailure _ -> return $ Left err
    ExitSuccess -> return $ Right out
