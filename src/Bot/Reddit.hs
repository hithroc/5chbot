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
import qualified Paths_5chbot as P
import Data.Version (showVersion)
import Bot.Csv
import Bot.Parse
import Bot.Util
import Bot.Config
import Bot.Drive

parseMessage :: Message -> Maybe Command
parseMessage msg = eitherToMaybe $ parse pCommand "" (body msg)

authorize :: (Monad m, MonadIO m) => Config -> Message -> RedditT m () -> RedditT m ()
authorize cfg msg m = do
  mods <- redditGetMods cfg
  if maybe False (`elem` mods) (from msg) then m else return ()

sendError :: (Monad m, MonadIO m) => Config -> Message -> Text.Text -> RedditT m ()
sendError cfg msg txt = case from msg of
  Nothing -> return ()
  Just u -> do
    let
      ans = body msg
          <>"\n\n-------------------\n\n"
          <>txt
    sendMessage u "Command error!" ans

execute :: (Monad m, MonadIO m) => Config -> Message -> Command -> RedditT m ()
execute cfg msg (Broadcast bcastMsg) = authorize cfg msg $ do
  tok <- liftIO $ initDrive (cfgGoogleId cfg) (cfgGoogleSecret cfg) "data/gcache"
  res <- liftIO $ downloadSpreadsheet tok (cfgMaillistId cfg)
  case res of
    Left e -> sendError cfg msg (Text.pack . show $ e)
    Right b -> do
      muser <- liftIO $ loadUsers b
      case muser of
        Nothing -> sendError cfg msg "Mailing list parse error!"
        Just users -> do
          liftIO $ print users
          broadcast (map Username users) (subject msg) bcastMsg

execute cfg msg (ErrorTest errMsg) = authorize cfg msg $ sendError cfg msg errMsg

execute cfg msg (Echo echoMsg) = maybe (return ()) (\u -> sendMessage u (subject msg) echoMsg) (from msg)
execute cfg msg (Version) = maybe (return ()) (\u -> sendMessage u "Version" (Text.pack $ ("5chbot " ++ showVersion P.version))) (from msg)
execute _ _ _ = return ()

broadcast :: (Monad m, MonadIO m) => [Username] -> Text.Text -> Text.Text -> RedditT m ()
broadcast users subject message = traverse_ (\u -> sendMessage u subject message) users

redditMain :: (Monad m, MonadIO m) => Config -> RedditT m ()
redditMain = redditLoop

redditGetMods :: Monad m => Config -> RedditT m ([Username])
redditGetMods cfg = return . map Username . cfgModerators $ cfg

redditLoop :: (Monad m, MonadIO m) => Config -> RedditT m ()
redditLoop cfg = do
  unread <- contents <$> getUnread
  let
    commands = catMaybes . map (\x -> (\y -> (x,y)) <$> parseMessage x) $ unread
  traverse_ markRead . map fst $ commands
  traverse_ (uncurry (execute cfg)) $ commands
  liftIO $ threadDelay 5000000 -- 5 sec
  redditLoop cfg
