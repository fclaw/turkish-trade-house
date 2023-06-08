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

-- taken from bootstrap (https://getbootstrap.com/docs/4.0/components/navbar/)
html =
    HH.div [HPExt.id "top"]
    [
        HH.nav [css "navbar navbar-expand-lg navbar-light bg-light"]
        [
            HH.div [css "collapse navbar-collapse", HPExt.id "navbarNavAltMarkup"]
            [
                HH.div [css "navbar-nav"]
                [HH.ul_ (map mkItem (fromEnum Home .. fromEnum Service) )]
            ]           
        ]
    ]