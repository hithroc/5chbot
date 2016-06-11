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
import Bot.Util
import Bot.Config
import Bot.Drive

parseMessage :: Message -> Maybe Command
parseMessage msg = eitherToMaybe $ parse pCommand "" (body msg)

authorize :: (Monad m, MonadIO m) => Message -> RedditT m () -> RedditT m ()
authorize msg m = do
  mods <- redditGetMods
  if maybe False (`elem` mods) (from msg) then m else return ()

execute :: (Monad m, MonadIO m) => Config -> Message -> Command -> RedditT m ()
execute cfg msg (Broadcast bcastMsg) = authorize msg $ do
  tok <- liftIO $ initDrive (googleId cfg) (googleSecret cfg) "data/gcache"
  res <- liftIO $ downloadSpreadsheet tok (maillistId cfg)
  case res of
    Left e -> return ()
    Right b -> do
      muser <- liftIO $ loadUsers b
      case muser of
        Nothing -> return ()
        Just users -> do
          liftIO $ print users
          broadcast (map Username users) (subject msg) bcastMsg
execute cfg msg (Echo echoMsg) = maybe (return ()) (\u -> sendMessage u (subject msg) echoMsg) (from msg)
execute _ _ _ = return ()

broadcast :: (Monad m, MonadIO m) => [Username] -> Text.Text -> Text.Text -> RedditT m ()
broadcast users subject message = traverse_ (\u -> sendMessage u subject message) users

redditMain :: (Monad m, MonadIO m) => Config -> RedditT m ()
redditMain = redditLoop

redditGetMods :: Monad m => RedditT m ([Username])
redditGetMods = return . map Username $ ["hithroc", "AnonymousHithroc", "iceman012"] -- For now

redditLoop :: (Monad m, MonadIO m) => Config -> RedditT m ()
redditLoop cfg = do
  unread <- contents <$> getUnread
  let
    commands = catMaybes . map (\x -> (\y -> (x,y)) <$> parseMessage x) $ unread
  traverse_ markRead . map fst $ commands
  traverse_ (uncurry (execute cfg)) $ commands
  liftIO $ threadDelay 5000000 -- 5 sec
  redditLoop cfg
