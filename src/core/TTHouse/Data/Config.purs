module TTHouse.Data.Config
  ( Config(..)
  , SendGrid(..)
  )  where

import Prelude

import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Argonaut.Decode.Error
import Data.Either

type SendGrid = 
     { apiKey :: String
     , spiHost :: String
     , spiUrl :: String
     }

sendGridToJson :: SendGrid -> Json
sendGridToJson = encodeJson

sendGridFromJson :: Json -> Either JsonDecodeError SendGrid
sendGridFromJson = decodeJson

type Config =
  { telegramBot :: String
  , telegramChat :: String
  , telegramHost :: String
  , toTelegram :: Boolean 
  , sendGrid :: SendGrid
  }

configToJson :: Config -> Json
configToJson = encodeJson

configFromJson :: Json -> Either JsonDecodeError Config
configFromJson = decodeJson