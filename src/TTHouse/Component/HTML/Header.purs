module TTHouse.Component.HTML.Header ( header ) where

import Prelude

import TTHouse.Component.HTML.Utils (css)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.Extended as HPExt
import Data.Tuple.Nested

header :: forall i p. HH.HTML i p
header = HH.div_ [HH.text "header"]