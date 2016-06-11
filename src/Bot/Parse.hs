module Bot.Parse where

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Text
import qualified Data.Text as Text

data Command
  = Broadcast Text.Text
  | Echo Text.Text
  | ErrorTest Text.Text
  | Version
  | Help
  deriving Show

commands :: [(Parser (), Parser Command)]
commands = map (\(x,y) -> (try . pStr $ x , y))
  [("broadcast", Broadcast <$> pContent)
  ,("echo", Echo <$> pContent)
  ,("error", ErrorTest <$> pContent)
  ,("version", return Version)
  ,("help", return Help)
  ]
  where
    pStr x = do
      string x
      space
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
