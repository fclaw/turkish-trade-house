module TTHouse.Component.HTML.Header ( html ) where

import Prelude

import TTHouse.Component.HTML.Menu.Hamburger as Hamburger 
import TTHouse.Component.HTML.Menu.Navbar as Navbar
import TTHouse.Component.HTML.Utils (css)
import TTHouse.Data.Route (Route)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.Extended as HPExt
import Store (Platform (..))

html :: forall i p. Route -> Platform -> Int -> HH.HTML i p
html r pl w = HH.div [css "page-header"] [showMenu r pl w]

showMenu r Mobile _ = Hamburger.html r
showMenu r _ w 
  | w > 500 = Navbar.html r
  | otherwise = Hamburger.html r