module Halogen.HTML.Properties.Extended
  ( module Properties
  , role
  )
  where

import Prelude

import Halogen.HTML.Properties as Properties
import Halogen.HTML as HH

role = HH.prop (HH.PropName "role")
