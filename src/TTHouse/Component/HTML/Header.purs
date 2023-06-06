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
                                        HH.a 
                                        [ HPExt.role "link" 
                                        , HPExt.aria_haspopup "menu"
                                        , HPExt.data_ux "Link"
                                        , HPExt.data_page "287f429b-ed6e-416e-aa2a-319409973b79"
                                        , HPExt.title "TTH"
                                        , css "x-el x-el-a c1-1o c1-1p c1-1q c1-1r c1-1s c1-1t c1-1u c1-1v c1-1w c1-1x c1-1y c1-z c1-1z c1-b c1-20 c1-c c1-21 c1-22 c1-23 c1-d c1-e c1-f c1-g"
                                        , HPExt.data_tccl "ux2.HEADER.header9.Logo.Default.Link.Default.134572.click,click"
                                        ]
                                        [
                                          HH.div 
                                          [ HPExt.data_ux "Block"
                                          , css "x-el x-el-div c1-1 c1-2 c1-24 c1-25 c1-26 c1-27 c1-28 c1-b c1-c c1-d c1-29 c1-e c1-f c1-g"
                                          ]
                                          [
                                            HH.img 
                                            [ HPExt.src "//img1.wsimg.com/isteam/ip/a0b31fcc-9849-4f2f-8c8c-f06192494b02/Original222.png/:/rs=h:200,cg:true,m/qt=q:95"
                                            , HPExt.srcSet 
                                              """ //img1.wsimg.com/isteam/ip/a0b31fcc-9849-4f2f-8c8c-f06192494b02/Original222.png/:/rs=w:475,h:200,cg:true,m/cr=w:475,h:200/qt=q:95, \
                                               //img1.wsimg.com/isteam/ip/a0b31fcc-9849-4f2f-8c8c-f06192494b02/Original222.png/:/rs=w:951,h:400,cg:true,m/cr=w:951,h:400/qt=q:95 2x, \
                                               //img1.wsimg.com/isteam/ip/a0b31fcc-9849-4f2f-8c8c-f06192494b02/Original222.png/:/rs=w:1426,h:600,cg:true,m/cr=w:1426,h:600/qt=q:95 3x 
                                              """
                                            , HPExt.alt "TTH"
                                            , HPExt.data_ux "ImageLogo"
                                            , HPExt.data_aid "HEADER_LOGO_IMAGE_RENDERED"
                                            , HPExt.id "n-134570"
                                            , css "x-el x-el-img c1-1 c1-2 c1-1z c1-z c1-v c1-w c1-2a c1-t c1-2b c1-2c c1-2d c1-2e c1-2f c1-2g c1-24 c1-25 c1-26 c1-27 c1-2h c1-28 c1-b c1-c c1-2i c1-2j c1-2k c1-2l c1-2m c1-2n c1-2o c1-2p c1-2q c1-2r c1-2s c1-2t c1-2u c1-2v c1-d c1-2w c1-2x c1-2y c1-e c1-f c1-g"
                                            ]
                                          ]
                                        ]
                                      ]   
                                   ]
                                , HH.div [HPExt.data_ux "Block", css "x-el x-el-div c1-1 c1-2 c1-1d c1-b c1-c c1-d c1-e c1-f c1-g"] []   
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

