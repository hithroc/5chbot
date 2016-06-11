module Bot.Drive where

import Data.Monoid
import Network.Google.OAuth2
import Network.HTTP.Conduit hiding (responseBody)
import Network.HTTP.Types (hAuthorization)
import Network.Google.Drive
import Control.Monad.IO.Class

import Control.Monad (forM_)
import Data.Conduit (($$+-))

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Network.Wreq
import Control.Lens



initDrive :: String -> String -> FilePath -> IO (String)
initDrive clientId clientSecret path = do
  let
    client = OAuth2Client clientId clientSecret
    scopes = ["https://www.googleapis.com/auth/drive"]
  getAccessToken client scopes (Just path)

downloadSpreadsheet :: String ->  T.Text -> IO (Either ApiError BS.ByteString)
downloadSpreadsheet token fileId = runApi token $ do
  Just item <- getFile fileId

  let path = localPath item
  let uri = T.unpack $ (fileExportLinks . fileData $ item) HM.! "text/csv" -- fixme
  r <- liftIO $ get uri
  return $ r ^. responseBody
