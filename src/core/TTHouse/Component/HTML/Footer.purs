module TTHouse.Component.HTML.Footer ( html ) where

import Prelude

import TTHouse.Component.HTML.Utils (css)
import TTHouse.Component.Message as Message

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

html = 
  HH.div [css "page-footer", HP.style "text-align: center"] 
  [ HH.text "COPYRIGHT © 2023 TTH - ALL RIGHTS RESERVED."
  , HH.slot_ Message.proxy unit Message.component unit
  ]