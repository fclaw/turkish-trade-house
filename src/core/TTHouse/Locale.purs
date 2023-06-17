module TTHouse.Locale (class Menu, getMap) where

import Prelude

import TTHouse.Data.Route (Route)
import TTHouse.Component.Lang (Lang (..))

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Map as Map
import Effect (Effect)

import Undefined

class Menu route lang | route -> lang where 
  getMap :: NonEmptyArray route -> lang -> Effect (Map.Map Route String)


instance Menu Route Lang where
  getMap  _ _ = pure Map.empty