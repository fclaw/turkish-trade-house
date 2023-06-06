module TTHouse.Component.HTML.Header ( html ) where

import Prelude

import TTHouse.Component.HTML.Utils (css)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.Extended as HPExt

html :: forall i p. HH.HTML i p
html = HH.div [css "page-header", HP.style "text-align: center"] [HH.text "header"]