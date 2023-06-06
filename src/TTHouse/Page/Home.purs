module TTHouse.Page.Home
  ( Action(..)
  , component
  )
  where

import Prelude

import TTHouse.Component.HamburgerMenu (proxy)

import Halogen as H
import Halogen.HTML as HH
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document)
import Web.HTML (window)
import Type.Proxy (Proxy(..))

data Action = Initialize

component menu mkBody =
  H.mkComponent
    { initialState: identity
    , render: const $ HH.div_ [HH.slot_ proxy unit menu unit, mkBody content ]
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize 
      }
    }

handleAction Initialize = H.liftEffect $ window >>= document >>= setTitle "TTH" 

content = HH.text "home"