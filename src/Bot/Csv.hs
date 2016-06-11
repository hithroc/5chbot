module Bot.Csv where

import Control.Monad.Trans.Reader
import Control.Monad
import qualified Data.Text as Text
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Vector as V
import Data.Csv
import Bot.Util


loadUsers :: BS.ByteString -> IO (Maybe [Text.Text])
loadUsers str = do
    let
      decoded :: Maybe (V.Vector (Text.Text, Text.Text))
      decoded = eitherToMaybe . decode HasHeader $ str
    return . fmap (map snd . V.toList) $ decoded
    where
        handler :: E.SomeException -> IO (Maybe BS.ByteString)
        handler _ = return Nothing
