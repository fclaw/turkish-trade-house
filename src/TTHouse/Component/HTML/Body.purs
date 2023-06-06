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
mkBody header content =
 HH.div 
 [ css "layout layout-layout layout-layout-layout-27 locale-tr-TR lang-tr"
 , HPExt.id "layout-a-0-b-31-fcc-9849-4-f-2-f-8-c-8-c-f-06192494-b-02"
 ]
 [
    HH.div 
    [ css "x-el x-el-div x-el c1-1 c1-2 c1-3 c1-4 c1-5 c1-6 c1-7 c1-8 c1-9 c1-a c1-b c1-c c1-d c1-e c1-f c1-g c1-1 c1-2 c1-b c1-c c1-d c1-e c1-f c1-g"
    , HPExt.id "page-134569"
    , HPExt.data_ux "Page"
    ]
    [
        HH.div 
        [ css "x-el x-el-div page-inner c1-1 c1-2 c1-b c1-c c1-d c1-e c1-f c1-g"
        , HPExt.data_ux "Block"
        ]
        [ header, content ]
    ]
 ]