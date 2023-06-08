module TTHouse.Page.About
  ( Action(..)
  , component
  , proxy
  )
  where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document)
import Web.HTML (window)
import Type.Proxy (Proxy(..))
import Store (Platform)
import Data.Maybe
import Undefined

proxy = Proxy :: _ "about"

data Action = Initialize

type State = { winWidth :: Maybe Int, platform :: Maybe Platform }

component mkBody =
  H.mkComponent
    { initialState: const { winWidth: Nothing, platform: Nothing }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize 
      }
    }
    where 
      render {winWidth: Just w, platform: Just p} = 
        HH.div_ [mkBody p w content]
      render _ = undefined

handleAction Initialize = H.liftEffect $ window >>= document >>= setTitle "About | TTH" 

content = HH.text "about"
