module TTHouse.Component.HTML.Footer ( html ) where

import Prelude

import TTHouse.Component.HTML.Utils (css)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.Extended as HPExt
import Data.Tuple.Nested
import Halogen.HTML.Elements.Extended

html :: forall i p. HH.HTML i p
html = HH.div [css "page-footer", HP.style "text-align: center"] [HH.text "COPYRIGHT © 2023 TTH - ALL RIGHTS RESERVED."]