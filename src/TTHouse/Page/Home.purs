module TTHouse.Page.Home
  ( Action(..)
  , component
  , proxy
  )
  where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document)
import Web.HTML (window)
import Type.Proxy (Proxy(..))

proxy = Proxy :: _ "home"

data Action = Initialize

component mkBody =
  H.mkComponent
    { initialState: identity
    , render: const $ HH.div_ [mkBody content]
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize 
      }
    }

handleAction Initialize = H.liftEffect $ window >>= document >>= setTitle "TTH" 

content = HH.text "home"