module TTHouse.Data.Config ( Config(..) ) where

import Prelude

import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Argonaut.Decode.Error
import Data.Either

type Config =
  { telegramBot :: String
  , telegramChat :: String
  , telegramHost :: String
  , toTelegram :: Boolean 
  }

configToJson :: Config -> Json
configToJson = encodeJson

configFromJson :: Json -> Either JsonDecodeError Config
configFromJson = decodeJson