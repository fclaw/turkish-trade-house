module App.Data.Config
  ( Config(..)
  , LogLevel(..)
  , logLevelToString
  )
  where

import Prelude

import Data.Either
import Data.Argonaut.Decode.Class
import Data.Argonaut.Encode.Class
import Data.Argonaut.Core
import Data.Argonaut.Decode.Error
import Data.Maybe
import Data.Either
import Foreign.Object as Obj
import Data.Newtype
import Data.Argonaut.Decode.Combinators (getField)

-- | Let's start with some types necessary for the rest of the module. We're
-- | going to store a logging level in our store, so we'll define it quickly;
-- | we could also put it in a dedicated module.
data LogLevel = Dev | Prod

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

logLevelToString :: LogLevel -> String
logLevelToString =
  case _ of
    Dev -> "dev"
    Prod -> "prod"

instance encodeJsonLogLevel :: EncodeJson LogLevel where
  encodeJson = encodeJson <<< logLevelToString

logLevelFromString :: String -> Maybe LogLevel
logLevelFromString =
  case _ of
    "dev" -> Just Dev
    "prod" -> Just Prod
    _ -> Nothing

logLevelFromJson :: Json -> Either JsonDecodeError LogLevel
logLevelFromJson json = do
  s <- decodeJson json
  note (TypeMismatch "LogLevel") (logLevelFromString s)

instance decodeJsonLogLevel :: DecodeJson LogLevel where
  decodeJson = logLevelFromJson

type Config =
  { logLevel :: LogLevel
  , telegramBot :: String
  , telegramChat :: String
  , telegramHost :: String
  }

configToJson :: Config -> Json
configToJson = encodeJson

configFromJson :: Json -> Either JsonDecodeError Config
configFromJson = decodeJson