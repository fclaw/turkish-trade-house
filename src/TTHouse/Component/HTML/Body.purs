module TTHouse.Component.HTML.Body
  ( Body
  , BodyHtml(..)
  , Content
  , Footer
  , Header
  , mkBodyHtml
  )
  where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import TTHouse.Component.HTML.Utils (css)
import Halogen.HTML.Properties.Extended as HPExt


type Content i p = HH.HTML i p
type Header i p = HH.HTML i p 
type Body i p = HH.HTML i p
type Footer i p = HH.HTML i p

type BodyHtml = 
     { header :: forall i p . Header i p
     , footer :: forall i p . Footer i p 
     , hamburger :: forall i p . HH.HTML i p
     }

mkBodyHtml { header, footer, hamburger } content = 
  HH.div_ [ hamburger, header, footer, contentWrapper content ]

contentWrapper content =
  HH.table_
  [
      HH.thead_ 
      [
         HH.tr_ 
         [
             HH.td_ [HH.div [css "page-header-space"] []]
         ]
      ]
  ,   HH.tbody_
      [
         HH.tr_
         [
            HH.td_ [HH.div_ [content]] 
         ]
      ]
  ,   HH.tfoot_ 
      [
         HH.tr_ 
         [
           HH.td_ [HH.div [css "page-footer-space"] []]
         ]
      ]
  ]