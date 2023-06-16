module TTHouse.Component.HTML.Header ( html ) where

import Prelude

import TTHouse.Component.Menu.Hamburger as Hamburger 
import TTHouse.Component.Menu.Navbar as Navbar
import TTHouse.Component.HTML.Utils (css)
import TTHouse.Component.Lang as Lang

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.Extended as HPExt
import Store (Platform (..))

html route pl w = 
  HH.div 
  [css "page-header"] 
  [
      HH.div [css "header-wrapper"] 
      [ 
          HH.div [css "header-logo-wrapper"]
          [HH.div_ [HH.slot_ Lang.proxy unit Lang.component unit]]
      ,   showMenu route pl w
      ]
  ]

showMenu route Mobile _ = HH.slot_ Hamburger.proxy unit Hamburger.component route
showMenu route _ w 
  | w > 500 = HH.slot_ Navbar.proxy unit Navbar.component route
  | otherwise  = HH.slot_ Hamburger.proxy unit Hamburger.component route