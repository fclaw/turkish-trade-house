module TTHouse.Page.Service
  ( Action(..)
  , component
  , proxy
  )
  where

import Prelude

import TTHouse.Component.Menu.Hamburger as HamburgerMenu

import Halogen as H
import Halogen.HTML as HH
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document)
import Web.HTML (window)
import Type.Proxy (Proxy(..))

proxy = Proxy :: _ "service"

data Action = Initialize

component mkBody =
  H.mkComponent
    { initialState: identity
    , render: const $ HH.div_ [HH.slot_ HamburgerMenu.proxy unit HamburgerMenu.component unit, mkBody content ]
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize 
      }
    }

handleAction Initialize = H.liftEffect $ window >>= document >>= setTitle "Service | TTH" 

content = HH.text "service"