module Cache
  ( Cache
  , init
  , writeTranslation
  , readTranslation
  ) 
 where

import Prelude

import TTHouse.Api.Foreign.Scaffold (Translation)

import Data.Map as Map
import Data.Maybe (Maybe (..))

type TranslationValue = { value :: Translation, hash :: String }


type CacheImpl = { translation :: Maybe TranslationValue }

newtype Cache = Cache CacheImpl

instance Show Cache where
  show _ = "<Cache>"

init :: Cache
init = Cache { translation: Nothing }

writeTranslation :: Translation -> String -> Cache -> Cache
writeTranslation x hash (Cache impl) = Cache $ impl { translation = Just { value: x, hash: hash } }

readTranslation :: Cache -> Maybe TranslationValue
readTranslation (Cache {translation: val}) = val
