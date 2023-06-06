module TTHouse.Component.HamburgerMenu ( component, proxy ) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

proxy = Proxy :: _ "hamburgerMenu"

component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval: H.mkEval H.defaultEval
    }

render = HH.div_ [HH.text "hamburger"] 

