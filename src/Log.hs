module Log where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (ZonedTime, getZonedTime)
import GHC.Exts (IsList (toList))

data LogLevel = Debug | Info | Warn | Error deriving (Show)

data Log = Log
  { timestamp :: ZonedTime,
    message :: T.Text,
    context :: Map.Map String String
  }
  deriving (Show)
