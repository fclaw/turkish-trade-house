module TTH.Page.Service ( component ) where

import Prelude

import Halogen as H
import Halogen.HTML as HH

component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval: H.mkEval H.defaultEval
    }

render = HH.div_ [HH.text "service"] 
  