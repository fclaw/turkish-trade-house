module Cache
  ( Cache
  , init
  , writeMenu
  , readMenu
  , readTranslation
  , writeTranslation
  ) 
 where

import Prelude

import TTHouse.Data.Route (Route)
import TTHouse.Api.Foreign.Scaffold (Translation, getTranslatedContent)

import Data.Map as Map
import Data.Maybe (Maybe)

type Menu = { xs :: Map.Map String String }

type CacheImpl = { menu :: Menu, translation :: Map.Map Route Translation }

newtype Cache = Cache CacheImpl

instance Show Cache where
  show (Cache { menu, translation }) =
    let { xs } = menu 
    in "{ menu: " <> show xs <>
       "translation: " <> show translation <> "}"

init :: Cache
init = Cache { menu: {xs: Map.empty }, translation: Map.empty }

writeMenu :: Map.Map String String -> Cache -> Cache
writeMenu xs (Cache impl) = Cache $ impl { menu = { xs: xs } }

readMenu :: Cache -> Map.Map String String
readMenu (Cache { menu: { xs } }) = xs

writeTranslation :: Map.Map Route Translation -> Cache -> Cache
writeTranslation xs (Cache impl) = Cache $ impl { translation = xs }

readTranslation :: Route -> Cache -> Maybe String
readTranslation route (Cache { translation: xs }) = map getTranslatedContent $ Map.lookup route xs 
