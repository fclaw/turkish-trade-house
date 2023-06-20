module TTHouse.Component.Cookie.Foreign where

import Prelude

import TTHouse.Api.Foreign.Scaffold as Scaffold 

import Effect (Effect)
import Data.Maybe (Maybe (..))

foreign import set :: Scaffold.Cookie -> Effect Unit
foreign import getIml :: Maybe Scaffold.Cookie -> (Scaffold.Cookie -> Maybe Scaffold.Cookie) -> String -> Effect (Maybe Scaffold.Cookie)

get ::  String -> Effect (Maybe Scaffold.Cookie)
get = getIml Nothing Just