module Cache where

import Prelude

import Data.Map as Map

type Menu = { menu :: Map.Map String String  }

newtype Cache = Cache Menu

instance Show Cache where
  show (Cache { menu }) = "{ langMap: " <> show menu <> "}"

init :: Cache
init = Cache { menu: Map.empty }

write :: Map.Map String String -> Cache
write xs = Cache { menu: xs }

read :: Cache -> Map.Map String String
read (Cache { menu }) = menu
