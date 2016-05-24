module Bot.Reddit where

import Reddit
import Reddit.Types.Message
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Foldable
import Data.Maybe
import Control.Concurrent
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Bot.Csv

data Command 
  = Broadcast Text.Text Text.Text
  | Echo Username Text.Text
  deriving Show

parseCommand :: Message -> Maybe Command
parseCommand msg = case listToMaybe (Text.words . body $ msg) of
  Just "!broadcast" -> Just $ Broadcast (subject msg) (Text.unwords . tail . Text.words . body $ msg)
  Just "!echo" -> from msg >>= \u -> Just $ Echo u (Text.unwords . tail . Text.words . body $ msg)
  Just _ -> Nothing
  Nothing -> Nothing

execute :: (Monad m, MonadIO m) => Command -> RedditT m ()
execute (Broadcast topic message) = do
  users <- liftIO $ loadUsers "maillist.csv"
  broadcast (map Username users) topic message
execute (Echo u msg) = sendMessage u "Echo" msg
execute _ = return ()

broadcast :: Monad m => [Username] -> Text.Text -> Text.Text -> RedditT m ()
broadcast users topic message = traverse_ (\u -> sendMessage u topic message) users

redditMain :: (Monad m, MonadIO m) => RedditT m ()
redditMain = do
  mods <- redditGetMods
  redditLoop mods

redditGetMods :: Monad m => RedditT m ([Username])
redditGetMods = return [Username "hithroc", Username "AnonymousHithroc"] -- For now

redditLoop :: (Monad m, MonadIO m) => [Username] -> RedditT m ()
redditLoop mods = do
  unread <- contents <$> getUnread
  let msgs = filter (\x -> from x `elem` map Just mods) unread
      commands = catMaybes . map (\x -> (\y -> (x,y)) <$> parseCommand x) $ msgs
  traverse_ markRead . map fst $ commands
  traverse_ execute . map snd $ commands
  liftIO $ threadDelay 5000000 -- 5 sec
  redditLoop mods
