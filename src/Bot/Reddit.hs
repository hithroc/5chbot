module Bot.Reddit where

import Reddit
import Reddit.Types.Message
import Text.Parsec (parse)
import Text.Parsec.Text
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Foldable
import Data.Maybe
import Control.Concurrent
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Bot.Csv
import Bot.Parse

parseMessage :: Message -> Maybe Command
parseMessage msg = eitherToMaybe $ parse pCommand "" (body msg)
  where
    eitherToMaybe (Left _) = Nothing
    eitherToMaybe (Right x) = Just x

execute :: (Monad m, MonadIO m) => Message -> Command -> RedditT m ()
execute msg (Broadcast bcastMsg) = do
  (Just users) <- liftIO $ loadUsers "maillist.csv"
  broadcast (map Username users) (subject msg) bcastMsg
execute msg (Echo echoMsg) = maybe (return ()) (\u -> sendMessage u (subject msg) echoMsg) (from msg)
execute _ _ = return ()

broadcast :: Monad m => [Username] -> Text.Text -> Text.Text -> RedditT m ()
broadcast users subject message = traverse_ (\u -> sendMessage u subject message) users

redditMain :: (Monad m, MonadIO m) => RedditT m ()
redditMain = do
  mods <- redditGetMods
  redditLoop mods

redditGetMods :: Monad m => RedditT m ([Username])
redditGetMods = return . map Username $ ["hithroc", "AnonymousHithroc", "iceman012"] -- For now

redditLoop :: (Monad m, MonadIO m) => [Username] -> RedditT m ()
redditLoop mods = do
  unread <- contents <$> getUnread
  let msgs = filter (\x -> from x `elem` map Just mods) unread
      commands = catMaybes . map (\x -> (\y -> (x,y)) <$> parseMessage x) $ msgs
  traverse_ markRead . map fst $ commands
  traverse_ (uncurry execute) $ commands
  liftIO $ threadDelay 5000000 -- 5 sec
  redditLoop mods
