module TTHouse.Page.Home
  ( Action(..)
  , component
  , proxy
  )
  where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document, innerWidth)
import Web.HTML (window)
import Type.Proxy (Proxy(..))
import Store (Platform)
import Data.Maybe
import Undefined
import Halogen.Store.Monad (class MonadStore, getStore)

proxy = Proxy :: _ "home"

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
      render _ = HH.div_ []
      handleAction Initialize = do
        { platform } <- getStore
        w <- H.liftEffect $ window >>= innerWidth
        H.modify_ _ { platform = pure platform, winWidth = pure w }
        H.liftEffect $ window >>= document >>= setTitle "TTH" 

content = HH.text "home"