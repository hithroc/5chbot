{-# LANGUAGE OverloadedStrings #-}
import Reddit
import Reddit.Types.Post

import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Bot.Config
import Bot.Reddit
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Directory
import System.Process

main :: IO ()
main = do
  createDirectoryIfMissing True "data"
  mcfg <- loadConfig "config.json"
  case mcfg of
    Nothing -> putStrLn "Error: Failed to open config.json!"
    Just cfg -> do
      res <- runReddit (cfgUsername cfg) (cfgPassword cfg) (redditMain cfg)
      print res

tshow :: Show a => a -> Text.Text
tshow = Text.pack . show
