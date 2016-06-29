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
import System.Process
import System.Exit
import System.Directory
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Paths_5chbot as P
import Data.Version (showVersion)
import Bot.Parse
import Bot.Util
import Bot.Config

parseMessage :: Message -> Maybe Command
parseMessage msg = eitherToMaybe $ parse pCommand "" (body msg)

authorize :: (Monad m, MonadIO m) => Config -> Message -> RedditT m () -> RedditT m ()
authorize cfg msg m = do
  mods <- redditGetMods cfg
  if maybe False (`elem` mods) (from msg) then m else return ()

sendError :: (Monad m, MonadIO m) => Config -> Message -> Text.Text -> RedditT m ()
sendError cfg msg txt = do
  let
    ans = "### Request"
        <>"\n\n>" <> body msg
        <>"\n\n### Error Message"
        <>"\n\n>" <> txt
  void $ replyMessage msg ans

execute :: (Monad m, MonadIO m) => Config -> Message -> Command -> RedditT m ()
execute cfg msg (Broadcast bcastMsg) = authorize cfg msg $ do
  let script = "/scripts/spreadsheet_wrapper.py"
  path <- liftIO $ ((++script) <$> getCurrentDirectory)
  (c, ret, _) <- liftIO $ readCreateProcessWithExitCode (proc path [Text.unpack $ cfgMaillistId cfg, "get_subs"]) ""

  case c of
    ExitFailure _ -> sendError cfg msg "Failed to fetch mailing list!"
    ExitSuccess -> do
      let users = map (Username . Text.pack) . lines $ ret
      broadcast users (subject msg) bcastMsg

execute cfg msg (ErrorTest errMsg) = authorize cfg msg $ sendError cfg msg errMsg

execute cfg msg (Echo echoMsg) = void $ replyMessage msg echoMsg
execute cfg msg Version = void $ replyMessage msg (Text.pack $ ("5chbot " ++ showVersion P.version))
execute cfg msg Unsubscribe = case from msg of
  Nothing -> return ()
  Just (Username u) -> do
    let script = "/scripts/spreadsheet_wrapper.py"
    path <- liftIO $ ((++script) <$> getCurrentDirectory)
    (c, ret, _) <- liftIO $ readCreateProcessWithExitCode (proc path [Text.unpack $ cfgMaillistId cfg, "unsubscribe", Text.unpack u]) ""
    let
      ans = case c of
        ExitFailure _ -> "Failed to unsubscribe. Try again later or contact subreddit moderators!"
        ExitSuccess -> "You have been unsubscribed!"
    void $ replyMessage msg ans
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
