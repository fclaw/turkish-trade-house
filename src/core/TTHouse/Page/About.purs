module TTHouse.Page.About
  ( Action(..)
  , component
  , proxy
  )
  where

import Prelude

import TTHouse.Page.Subscription.WinResize as WinResize
import  TTHouse.Capability.LogMessages (logDebug)

import Halogen as H
import Halogen.HTML as HH
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document, innerWidth)
import Web.HTML (window)
import Type.Proxy (Proxy(..))
import Store (Platform)
import Data.Maybe
import Halogen.Store.Monad (getStore)
import System.Time (getTimestamp)
import Statistics (sendComponentTime) 

proxy = Proxy :: _ "about"

componentName = "TTHouse.Page.About"

data Action = Initialize | WinResize Int | Finalize

type State = { winWidth :: Maybe Int, platform :: Maybe Platform, start :: Int }

component mkBody =
  H.mkComponent
    { initialState: const { winWidth: Nothing, platform: Nothing, start: 0 }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize 
      , finalize = pure Finalize
      }
    }
    where 
      render {winWidth: Just w, platform: Just p} = 
        HH.div_ [mkBody p w content]
      render _ = HH.div_ []
      handleAction Initialize = do
        H.liftEffect $ window >>= document >>= setTitle "About | TTH"
        { platform } <- getStore
        w <- H.liftEffect $ window >>= innerWidth

        tm <- H.liftEffect getTimestamp

        logDebug $ "(" <> componentName <> ") component has started at " <> show tm

        H.modify_ _ { platform = pure platform, winWidth = pure w, start = tm }
        void $ H.subscribe =<< WinResize.subscribe WinResize
      handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }

      handleAction Finalize = do 
        end <- H.liftEffect getTimestamp
        {start} <- H.get
        sendComponentTime start end componentName

content = HH.text "about"
