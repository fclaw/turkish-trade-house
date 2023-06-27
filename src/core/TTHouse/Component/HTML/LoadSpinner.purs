-- https://codepen.io/DHawku/pen/yXEZdq
module TTHouse.Component.HTML.LoadSpinner (html) where

import Prelude

import TTHouse.Component.HTML.Utils (css)

import Halogen.HTML as HH

html = 
  HH.div [css "loading-container"]
  [
      HH.div [css "spinner"] []
  ,   HH.div [css "spinner-center"] []
  ,   HH.div [css "loading-text"] [HH.text "app is loading ..."]
  ]


