module Cache where

import Prelude

import Data.Map as Map

type Menu = { langMap :: Map.Map String String  }

newtype Cache = Cache Menu

instance Show Cache where
  show (Cache { langMap }) = "{ langMap: " <> show langMap <> "}"

init :: Cache
init = Cache { langMap: Map.empty }

write :: Map.Map String String -> Cache
write xs = Cache { langMap: xs }

read :: Cache -> Map.Map String String
read (Cache { langMap }) = langMap
