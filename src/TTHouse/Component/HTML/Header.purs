module TTHouse.Component.HTML.Header ( header ) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.Extended as HPExt
import TTHouse.Component.HTML.Utils (css)

header :: forall i p. HH.HTML i p
header = 
  HH.div 
  [ HPExt.id "71a5b96d-5d09-4403-9988-2116efa4263f"
  , css "widget widget-header widget-header-header-9"
  ] 
  [
    HH.div 
    [ HPExt.data_ux "Header"
    , HPExt.role "main" 
    , HPExt.data_aid "HEADER_WIDGET" 
    , HPExt.id "n-134570"
    , css "x-el x-el-div x-el x-el c1-1 c1-2 c1-b c1-c c1-d c1-e c1-f c1-g c1-1 c1-2 c1-3 c1-b c1-c c1-d c1-e c1-f c1-g c1-1 c1-2 c1-b c1-c c1-d c1-e c1-f c1-g"
    ]
    [
        HH.div_
        [
            HH.section 
            [ HPExt.data_ux "Section" 
            , HPExt.data_aid "HEADER_SECTION"
            , css "x-el x-el-section c1-1 c1-2 c1-3 c1-h c1-i c1-j c1-b c1-c c1-k c1-l c1-m c1-n c1-o c1-p c1-q c1-r c1-d c1-e c1-f c1-g"
            ]
            [
              HH.div 
                [ HPExt.data_ux "Block"
                , css "x-el x-el-div c1-1 c1-2 c1-s c1-t c1-b c1-c c1-u c1-d c1-e c1-f c1-g"
                ]
                [
                   HH.div 
                   [ HPExt.data_ux "Block"
                   , css "x-el x-el-div c1-1 c1-2 c1-b c1-c c1-d c1-e c1-f c1-g"
                   ]
                   [
                      HH.div [HPExt.data_ux "ContainerFluid"]
                      [
                         HH.div [HPExt.data_ux "Block", css "x-el x-el-div c1-1 c1-2 c1-10 c1-b c1-c c1-d c1-11 c1-e c1-f c1-g"] 
                         [
                            HH.nav [HPExt.data_ux "Container", css "x-el x-el-nav c1-1 c1-2 c1-v c1-w c1-x c1-y c1-z c1-4 c1-12 c1-13 c1-b c1-c c1-14 c1-d c1-15 c1-e c1-16 c1-f c1-17 c1-g"] 
                            [
                                HH.div [HPExt.data_ux "Block", css "x-el x-el-div c1-1 c1-2 c1-18 c1-19 c1-1a c1-1b c1-1c c1-b c1-c c1-d c1-e c1-f c1-g"]
                                [
                                   HH.div [HPExt.data_ux "Block", css "x-el x-el-div c1-1 c1-2 c1-1d c1-b c1-c c1-d c1-e c1-f c1-g"] [] 
                                ,  HH.div [HPExt.data_ux "Block", css "x-el x-el-div c1-1 c1-2 c1-1e c1-1f c1-1g c1-b c1-c c1-d c1-e c1-f c1-g"] 
                                   [
                                      HH.div [HPExt.data_ux "Block", HPExt.data_aid "HEADER_LOGO_RENDERED", css "x-el x-el-div c1-1h c1-1i c1-1j c1-c c1-1k c1-1l c1-1m c1-1n c1-d c1-e c1-f c1-g"]
                                      [
                                        HH.text "test"
                                      ]
                                   ]
                                ]
                            ]
                         ]
                      ,  HH.div [HPExt.data_ux "Block", css "x-el x-el-div c1-1 c1-2 c1-10 c1-b c1-c c1-d c1-11 c1-e c1-f c1-g"] []
                      ]
                   ]   
                ]
            ]
        ]
      ]
    ]

