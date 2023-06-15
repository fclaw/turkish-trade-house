module TTHouse.Component.HTML.Header ( html ) where

import Prelude

import TTHouse.Component.HTML.Menu.Hamburger as Hamburger 
import TTHouse.Component.HTML.Menu.Navbar as Navbar
import TTHouse.Component.HTML.Utils (css)
import TTHouse.Data.Route (Route)
import TTHouse.Component.Lang as Lang

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.Extended as HPExt
import Store (Platform (..))

html r pl w = HH.div [css "page-header"] [showMenu r pl w]

showMenu r Mobile _ = Hamburger.html r
showMenu r _ w 
  | w > 500 = 
      HH.div [css "header-wrapper"] 
      [ HH.div [css "header-logo-wrapper"] []
      , Navbar.html r
      , HH.slot_ Lang.proxy unit Lang.component unit
      ]
  | otherwise = Hamburger.html r