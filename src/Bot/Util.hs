module Bot.Util where

import Paths_5chbot
import qualified Data.Version as V

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a

showVersion :: String
showVersion = "5chbot ver. " ++ V.showVersion version

