module Bot.Config where

import Control.Monad.Trans.Reader
import Data.Aeson
import qualified Data.Text as Text
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as BS

data Config = Config
  { userName :: Text.Text
  , password :: Text.Text
  , googleId :: String
  , googleSecret :: String
  , maillistId :: Text.Text
  }
  deriving Show

instance FromJSON Config where
  parseJSON (Object v) = Config 
    <$> v .: "username"
    <*> v .: "password"
    <*> v .: "google_client_id"
    <*> v .: "google_client_secret"
    <*> v .: "google_maillist_id"


loadConfig :: FilePath -> IO (Maybe Config)
loadConfig path = do
    str <- (Just <$> BS.readFile path) `E.catch` handler
    return $ str >>= decode
    where
        handler :: E.SomeException -> IO (Maybe BS.ByteString)
        handler _ = return Nothing
