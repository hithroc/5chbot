module Bot.Core where

import Reddit
import Reddit.Types.Message
import Text.Parsec (parse)
import Text.Parsec.Text
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Map as M
import Data.Monoid
import Data.Foldable
import Data.Maybe
import Data.List
import Control.Concurrent
import qualified Network.Wreq as W
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Applicative
import System.Log.Logger
import Bot.Parse
import Bot.Util
import Bot.Config
import Bot.Script
import Settings
import Control.Exception

sendWebhook :: Config -> Text.Text -> IO ()
sendWebhook cfg msg = do
  let
    m :: M.Map Text.Text Text.Text
    m = M.fromList [("content", msg)]
  case cfgDiscordWebHook cfg of
    Just hook -> do
      er <- try $ W.post (Text.unpack hook) (toJSON m)
      case er of
        Left (SomeException e) -> print e
        Right _ -> return ()
    Nothing -> return ()

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
      let users = nub . map (Username . Text.pack) . lines $ out
      liftIO $ sendWebhook cfg bcastMsg
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
broadcast users subject message = do
  traverse_ f users
  where
    botdec = Text.pack . ("\n\n"++) . unwords . map ("^^"++) . words $ botrem
    botrem = "I am a bot. If you want to unsubscribe from the broadcast messages reply with \"!unsubscribe\""
    f :: (Monad m, MonadIO m) => Username -> RedditT m ()
    f u = do
      liftIO . infoM rootLoggerName $ "Sending a message to " ++ show u
      re <- nest $ sendMessage u subject (message<>botdec)
      case re of
        Right _ -> return ()
        Left (APIError e) -> liftIO . noticeM rootLoggerName $ "Broadcast sendmessage exception for " ++ show u ++ ": " ++ show e

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
