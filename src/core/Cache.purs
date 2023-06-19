module Cache
  ( Cache
  , init
  , writeMenu
  , readMenu
  ) 
 where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe (Nothing))

type Menu = { xs :: Map.Map String String }

type Home = { body :: String }

type CacheImpl = { menu :: Menu, home :: Maybe Home }

newtype Cache = Cache CacheImpl

instance Show Cache where
  show (Cache { menu, home }) =
    let { xs } = menu 
    in "{ menu: " <> show xs <>
       "home: " <> show home <> "}"

init :: Cache
init = Cache { menu: {xs: Map.empty }, home: Nothing }

writeMenu :: Map.Map String String -> Cache -> Cache
writeMenu xs (Cache impl) = Cache $ impl { menu = { xs: xs } }

readMenu :: Cache -> Map.Map String String
readMenu (Cache { menu: { xs } }) = xs
