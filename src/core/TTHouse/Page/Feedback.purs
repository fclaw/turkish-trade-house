module TTHouse.Page.Feedback
  ( Action(..)
  , component
  , proxy
  )
  where

import Prelude

import TTHouse.Page.Subscription.WinResize as WinResize
import TTHouse.Component.Message as Message 

import Halogen as H
import Halogen.HTML as HH
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document, innerWidth)
import Web.HTML (window)
import Type.Proxy (Proxy(..))
import Store (Platform)
import Data.Maybe
import Halogen.Store.Monad (getStore)

proxy = Proxy :: _ "feedback"

data Action = Initialize | WinResize Int

type State = { winWidth :: Maybe Int, platform :: Maybe Platform  }

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
        void $ H.subscribe =<< WinResize.subscribe WinResize
      handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }

handleAction Initialize = H.liftEffect $ window >>= document >>= setTitle "Feedback | TTH" 

content = HH.slot_ Message.proxy unit Message.component unit