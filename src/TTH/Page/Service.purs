module TTH.Page.Service ( component ) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document)
import Web.HTML (window)

data Action = Initialize

component =
  H.mkComponent
    { initialState: identity
    , render: const  render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize 
      }
    }

handleAction Initialize = H.liftEffect $ window >>= document >>= setTitle "Service | TTH" 

render = HH.div_ [HH.text "service"] 
  