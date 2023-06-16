module TTHouse.Page.Error
  ( component
  , proxy
  )
  where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

proxy = Proxy :: _ "error"

component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval: H.mkEval H.defaultEval
    }

render = HH.div_ [HH.text "error"]