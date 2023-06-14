module TTHouse.Component.HTML.Menu.Hamburger ( html, mkItem ) where

import Prelude

import TTHouse.Component.HTML.Utils (css, safeHref)
import TTHouse.Data.Route (Route (..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Data.Array ((..))
import Data.Enum (fromEnum, toEnum)
import DOM.HTML.Indexed.InputType
import Data.Maybe
import Undefined

-- I piggyback on the following implementation https://codepen.io/alvarotrigo/pen/PoJGObg
html r =
    HH.div_
    [
        HH.input [HPExt.type_ InputCheckbox, css "toggler"]
    ,   HH.div [css "hamburger"] [HH.div_ []]
    ,   HH.div [css "menu"]
        [
            HH.div [HPExt.style "#position: relative; #top: -50%;margin:0 auto;width:200px"] 
            [HH.ul_ (map (mkItem r) (fromEnum Home .. fromEnum Feedback) )]
        ]     
    ]

mkItem route idx = HH.li_ [HH.a [safeHref (mkRoute idx), isDisabled (mkRoute idx == route)] [HH.text (show (mkRoute idx))] ] 
  where 
    mkRoute = fromMaybe undefined <<< (toEnum :: Int -> Maybe Route)
    isDisabled true = HPExt.style "cursor: not-allowed;"
    isDisabled false = HPExt.style mempty