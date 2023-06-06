module TTHouse.Component.HTML.Body
  ( Body
  , Content
  , Header
  , mkBodyHtml
  )
  where

import Prelude

import Halogen.HTML as HH
import TTHouse.Component.HTML.Utils (css)
import Halogen.HTML.Properties.Extended as HPExt


type Content i p = HH.HTML i p
type Header i p = HH.HTML i p 
type Body i p = HH.HTML i p
type Footer i p = HH.HTML i p

mkBodyHtml :: forall i p. Header i p -> Footer i p -> Content i p -> Body i p
mkBodyHtml header footer content = HH.div_ [ header, footer, contentWrapper content ]

contentWrapper content =
  HH.table_ 
  [
     HH.thead_ 
     [
        HH.tr_ 
        [
           HH.td_ 
           [
              HH.div [css "page-header-space"] []
           ]
        ]
     ]
  ,  HH.tbody_
     [
        HH.tr_
        [
           HH.td_
           [
              HH.div_ [ content ]
           ] 
        ]
     ]
  ,  HH.tfoot_ 
     [
        HH.tr_ 
        [
          HH.td_ 
          [
             HH.div [css "page-footer-space"] []
          ]
        ]
     ]
  ]