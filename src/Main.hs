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

main :: IO ()
main = do
  mcfg <- loadConfig "config.json"
  case mcfg of
    Nothing -> putStrLn "Error: Failed to open config.json!"
    Just cfg -> do
      res <- runReddit (userName cfg) (password cfg) redditMain
      print res

tshow :: Show a => a -> Text.Text
tshow = Text.pack . show
