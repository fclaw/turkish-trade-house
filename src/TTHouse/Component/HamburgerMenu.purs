module TTHouse.Component.HamburgerMenu ( component, proxy ) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import DOM.HTML.Indexed.InputType
import Halogen.HTML.Properties.Extended as HPExt
import TTHouse.Component.HTML.Utils (css, safeHref)
import TTHouse.Data.Route (Route (..))
import Routing.Duplex (print)
import Data.Array ((..))
import Data.Enum (fromEnum, toEnum)
import Data.Maybe
import Undefined

proxy = Proxy :: _ "hamburgerMenu"

component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval: H.mkEval H.defaultEval
    }


-- I piggyback on the following implementation https://codepen.io/alvarotrigo/pen/PoJGObg
render = 
  HH.div_ 
  [
     HH.input [HPExt.type_ InputCheckbox, css "toggler"]
  ,  HH.div [css "hamburger"] [HH.div_ []]
  ,  HH.div [css "menu"]
     [
        HH.div_
        [
            HH.ul_ (map item (fromEnum Home .. fromEnum Service) )
        ]
     ]  
  ]

item route = HH.li_ [HH.a [safeHref (fromMaybe Home ((toEnum :: Int -> Maybe Route) route)) ] [HH.text (show (fromMaybe undefined ((toEnum :: Int -> Maybe Route) route)))] ] 
