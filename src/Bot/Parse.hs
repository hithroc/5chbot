module Bot.Parse where

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Text
import Control.Monad
import qualified Data.Text as Text

data Command
  = Broadcast Text.Text
  | Echo Text.Text
  | ErrorTest Text.Text
  | Version
  | Unsubscribe
  | Help
  deriving Show

cmdName :: Command -> String
cmdName (Broadcast _) = "broadcast"
cmdName (Echo _) = "echo"
cmdName (ErrorTest _) = "error_test"
cmdName Version = "version"
cmdName Unsubscribe = "unsubscribe"
cmdName Help = "help"

commands :: [(Parser (), Parser Command)]
commands = map (\(x,y) -> (try . pStr $ x , y))
  [("broadcast", Broadcast <$> pContent)
  ,("echo", Echo <$> pContent)
  ,("error", ErrorTest <$> pContent)
  ,("version", return Version)
  ,("help", return Help)
  ,("unsubscribe", return Unsubscribe)
  ]
  where
    pStr x = do
      string x
      try (void space) <|> eof
      return ()

pContent :: Parser Text.Text
pContent = Text.pack <$> manyTill anyChar eof

pCommand :: Parser Command
pCommand = do
  char '!'
  let
    f ((x,y):xs) = do 
      m <- optionMaybe . try $ x
      case m of
        Just _ -> y
        Nothing -> f xs
    f [] = unexpected "command"
  f commands
