module Bot.Core where

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
import System.Log.Logger
import Bot.Parse
import Bot.Util
import Bot.Config
import Bot.Script
import Settings

parseMessage :: Message -> Maybe Command
parseMessage msg = eitherToMaybe $ parse pCommand "" (body msg)

authorize :: (Monad m, MonadIO m) => Config -> Message -> RedditT m () -> RedditT m ()
authorize cfg msg m = do
  mods <- redditGetMods cfg
  if maybe False (`elem` mods) (from msg)
  then m 
  else do
    liftIO $ noticeM rootLoggerName $ "Unauthorized request from " 
                                    ++ show (from msg)
                                    ++ "! Message body: \""
                                    ++ Text.unpack (body msg) ++ "\""
    return ()

sendError :: (Monad m, MonadIO m) => Config -> Message -> Text.Text -> RedditT m ()
sendError cfg msg txt = do
  liftIO $ noticeM rootLoggerName $ "A request produced an error! "
                                 ++ "Request message: \"" ++ Text.unpack (body msg) ++ "\""
  let
    ans = "### Request"
        <>"\n\n>" <> body msg
        <>"\n\n### Error Message"
        <>"\n\n>" <> txt
  void $ replyMessage msg ans

execute :: (Monad m, MonadIO m) => Config -> Message -> Command -> RedditT m ()
execute cfg msg (Broadcast bcastMsg) = authorize cfg msg $ do
  liftIO $ infoM rootLoggerName "Fetching mailing list..."
  ret <- liftIO $ wrapperScript cfg ["get_subs"]
  case ret of
    Left err -> do
      let errMessage = "Failed to fetch mailing list!\n" ++ err
      liftIO $ errorM rootLoggerName (errMessage)
      sendError cfg msg (Text.pack errMessage)
    Right out -> do
      let users = map (Username . Text.pack) . lines $ out
      broadcast users (subject msg) bcastMsg

execute cfg msg (ErrorTest errMsg) = authorize cfg msg $ sendError cfg msg errMsg

execute cfg msg (Echo echoMsg) = void $ replyMessage msg echoMsg
execute cfg msg Version = void $ replyMessage msg (Text.pack $ showVersion)
execute cfg msg Unsubscribe = case from msg of
  Nothing -> return ()
  Just (Username u) -> do
    ret <- liftIO $ wrapperScript cfg ["unsubscribe", Text.unpack u]
    ans <- case ret of
        Left err -> do
          liftIO $ warningM rootLoggerName $ "Failed to unsubscribe " ++ show u ++ ": " ++ err
          return "Failed to unsubscribe. Try again later or contact subreddit moderators!"
        Right out -> return "You have been unsubscribed!"
    void $ replyMessage msg ans
execute _ _ _ = return ()

broadcast :: (Monad m, MonadIO m) => [Username] -> Text.Text -> Text.Text -> RedditT m ()
broadcast users subject message = traverse_ (\u -> sendMessage u subject message) users

redditMain :: (Monad m, MonadIO m) => Config -> RedditT m ()
redditMain cfg = do
  liftIO . infoM rootLoggerName $ "Start of the main loop."
  redditLoop cfg

redditGetMods :: Monad m => Config -> RedditT m ([Username])
redditGetMods cfg = return . map Username . cfgModerators $ cfg

redditLoop :: (Monad m, MonadIO m) => Config -> RedditT m ()
redditLoop cfg = do
  unread <- contents <$> getUnread
  when (not $ null unread) . liftIO $ debugM rootLoggerName (show unread)
  let
    commands = catMaybes . map (\x -> (\y -> (x,y)) <$> parseMessage x) $ unread
  liftIO . when (not $ null commands) $ do
    infoM rootLoggerName $ "Found " ++ show (length commands) ++ " new command requests: " ++ show (map (cmdName . snd) commands)
  traverse_ markRead . map fst $ commands
  traverse_ (uncurry (execute cfg)) $ commands
  liftIO $ threadDelay 5000000 -- 5 sec
  redditLoop cfg
