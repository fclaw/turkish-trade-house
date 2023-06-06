module Halogen.HTML.Properties.Extended
  ( aria_haspopup
  , data_aid
  , data_page
  , data_tccl
  , data_ux
  , module Properties
  , role
  , srcSet
  )
  where

import Prelude

import Halogen.HTML.Properties as Properties
import Halogen.HTML as HH

data_ux = HH.prop (HH.PropName "data-ux")
data_aid = HH.prop (HH.PropName "data-aid")
role = HH.prop (HH.PropName "role")
aria_haspopup = HH.prop (HH.PropName "aria-haspopup")
data_page = HH.prop (HH.PropName "data-page")
data_tccl = HH.prop (HH.PropName "data-tccl")
srcSet = HH.prop (HH.PropName "srcSet")