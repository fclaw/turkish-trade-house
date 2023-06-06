module Halogen.HTML.Properties.Extended
  ( data_aid
  , data_ux
  , module Properties
  , role
  )
  where

import Prelude

import Halogen.HTML.Properties as Properties
import Halogen.HTML as HH

data_ux = HH.prop (HH.PropName "data-ux")
data_aid = HH.prop (HH.PropName "data-aid")
role = HH.prop (HH.PropName "role")