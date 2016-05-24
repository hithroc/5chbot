module Bot.Csv where

import Control.Monad.Trans.Reader
import Control.Monad
import qualified Data.Text as Text
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Data.Csv

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a

loadUsers :: FilePath -> IO (Maybe [Text.Text])
loadUsers path = do
    str <- (Just <$> BS.readFile path) `E.catch` handler
    let
      decoded :: Maybe (V.Vector (Text.Text, Text.Text))
      decoded = join . fmap (eitherToMaybe . decode HasHeader) $ str
    return . fmap (map snd . V.toList) $ decoded
    where
        handler :: E.SomeException -> IO (Maybe BS.ByteString)
        handler _ = return Nothing
