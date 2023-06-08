module TTHouse.Component.HTML.Header ( html ) where

import Prelude

import TTHouse.Component.HTML.Menu.Hamburger as Hamburger 
import TTHouse.Component.HTML.Menu.Navbar as Navbar
import TTHouse.Component.HTML.Utils (css)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.Extended as HPExt
import Store (Platform (..))

html :: forall i p. Platform -> Int -> HH.HTML i p
html pl w = HH.div [css "page-header"] [showMenu pl w]

showMenu Mobile _ = Hamburger.html
showMenu _ w 
  | w > 500 = Navbar.html
  | otherwise = Hamburger.html