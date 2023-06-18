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
import Data.Map as Map

html route lang pl w = 
  HH.div 
  [css "page-header"] 
  [
      HH.div [css "header-wrapper"] 
      [ 
          HH.div [css "header-logo-wrapper"]
          [HH.div_ [HH.slot_ Lang.proxy unit Lang.component unit]]
      ,   showMenu route lang pl w
      ]
  ]

showMenu route lang Mobile _ = HH.slot_ Hamburger.proxy unit Hamburger.component { route: route, lang: lang, routesTitle: Map.empty }
showMenu route lang _ w 
  | w > 500 = HH.slot_ Navbar.proxy unit Navbar.component { route: route, lang: lang, routesTitle: Map.empty }
  | otherwise  = HH.slot_ Hamburger.proxy unit Hamburger.component { route: route, lang: lang, routesTitle: Map.empty }