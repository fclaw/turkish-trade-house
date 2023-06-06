module TTHouse.Component.HTML.Body
  ( Body
  , Content
  , Header
  , mkBody
  )
  where

import Prelude

import Halogen.HTML as HH
import TTHouse.Component.HTML.Utils (css)
import Halogen.HTML.Properties.Extended as HPExt


type Content i p = HH.HTML i p
type Header i p = HH.HTML i p 
type Body i p = HH.HTML i p

mkBody :: forall i p. Header i p -> Content i p -> Body i p
mkBody header content = HH.div_ [ header, content ]