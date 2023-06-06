module TTHouse.Component.HTML.Header ( header ) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Undefined


header :: forall i p. HH.HTML i p
header = HH.div_ [HH.text "header"]

