module TTHouse.Component.HTML.Menu.Navbar ( html ) where

import Prelude

import TTHouse.Component.HTML.Utils (css)
import TTHouse.Data.Route (Route (..))
import TTHouse.Component.HTML.Menu.Hamburger (mkItem)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Data.Array ((..))
import Data.Enum (fromEnum, toEnum)
import DOM.HTML.Indexed.InputType

-- taken from: https://codepen.io/albizan/pen/mMWdWZ
html r =
  HH.div [css "wrapper"]
  [ 
      HH.nav [css "navbar navbar-expand-lg navbar-light"]
      [
          HH.ul [css "navbar-nav"] (map (mkItem r) (fromEnum Home .. fromEnum Service) ) 
      ]
  ]   