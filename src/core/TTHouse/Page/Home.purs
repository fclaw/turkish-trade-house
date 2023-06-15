module TTHouse.Page.Home
  ( Action(..)
  , component
  , proxy
  )
  where

import Prelude

import TTHouse.Page.Subscription.WinResize as WinResize 
import TTHouse.Api.Foreign.Scaffold as Scaffold
import TTHouse.Component.Lang (Lang)
import  TTHouse.Capability.LogMessages (logDebug)
import TTHouse.Component.Lang (LangVar (..), Recipients (Home))

import Halogen as H
import Halogen.HTML as HH
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document, innerWidth)
import Web.HTML (window)
import Type.Proxy (Proxy(..))
import Store (Platform)
import Data.Maybe
import Halogen.Store.Monad (getStore)
import Concurrent.Channel as Async
import Control.Monad.Rec.Class (forever)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Data.Foldable (for_)
import Effect.AVar as Async

proxy = Proxy :: _ "home"

data Action = Initialize | WinResize Int | LangChange Lang

type State = 
     { winWidth :: Maybe Int
     , platform :: Maybe Platform
     , body :: String 
     }

component mkBody =
  H.mkComponent
    { initialState: const { winWidth: Nothing, platform: Nothing, body: mempty }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize 
      }
    }
    where 
      render {winWidth: Just w, platform: Just p, body: body} = 
        HH.div_ [mkBody p w (content body) ]
      render _ = HH.div_ []
      handleAction Initialize = do
        H.liftEffect $ window >>= document >>= setTitle "TTH"
        { platform, init, langChannel } <- getStore
        w <- H.liftEffect $ window >>= innerWidth
        H.modify_ _ { platform = pure platform, winWidth = pure w, body = Scaffold.getHomeContent init }
        H.liftEffect $ window >>= document >>= setTitle "TTH" 
        void $ H.subscribe =<< WinResize.subscribe WinResize

        void $ H.fork $ forever $ do
          langm <- H.liftAff do
            Aff.delay $ Milliseconds 500.0
            Async.recv $ _.input langChannel
          for_ langm \x@{ recipients, lang } ->
            case recipients of 
              Home -> handleAction $ LangChange lang
              _ -> void $ H.liftAff $ Async.send (_.output langChannel) x

      handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }
      handleAction (LangChange lang) =
        logDebug $ "(TTHouse.Page.Home) language change to: " <> show lang

content = HH.text