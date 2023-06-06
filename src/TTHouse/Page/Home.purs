module TTHouse.Page.Home
  ( Action(..)
  , component
  )
  where

import Prelude

import TTHouse.Capability.LogMessages

import Halogen as H
import Halogen.HTML as HH
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document)
import Web.HTML (window)

data Action = Initialize

component mkBody =
  H.mkComponent
    { initialState: identity
    , render: const $ mkBody content
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize 
      }
    }

handleAction Initialize = H.liftEffect $ window >>= document >>= setTitle "TTH" 

content = HH.text "home"