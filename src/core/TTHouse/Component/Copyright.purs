module TTHouse.Component.Copyright (component, proxy) where

import Prelude

import TTHouse.Component.HTML.Utils (css)

import Halogen as H
import Halogen.HTML as HH
import Undefined
import Type.Proxy (Proxy (..))

proxy = Proxy :: _ "copyright"

data Action = Initialize | LangChange String String

component =
  H.mkComponent
    { initialState: identity
    , render: render
    , eval: H.mkEval H.defaultEval 
      { initialize = pure Initialize
      , handleAction = \action ->
          case action of 
            Initialize -> pure unit
            LangChange _ _  -> undefined 
      }
    }

render _ = HH.div [css "copyright-plaque"] [HH.text "COPYRIGHT © 2023 TTH - ALL RIGHTS RESERVED."]