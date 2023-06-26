module TTHouse.Page.Home
  ( Action(..)
  , component
  , proxy
  )
  where

import Prelude

import TTHouse.Page.Subscription.WinResize as WinResize 
import TTHouse.Api.Foreign.Scaffold as Scaffold
import TTHouse.Component.Lang.Data (Lang (..))
import TTHouse.Capability.LogMessages (logDebug)
import TTHouse.Component.Lang.Data (Recipients (Home))
import TTHouse.Api.Foreign.Request as Request
import TTHouse.Data.Route as Route
import TTHouse.Api.Foreign.Request.Handler (withError)
import TTHouse.Document.Meta as Meta
import TTHouse.Component.Utils (initTranslation)
import TTHouse.Component.Subscription.Translation as Translation

import Halogen as H
import Halogen.HTML as HH
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document, innerWidth)
import Web.HTML (window)
import Type.Proxy (Proxy(..))
import Store (Platform, Action (WriteError))
import Data.Maybe
import Halogen.Store.Monad (getStore)
import Control.Monad.Rec.Class (forever)
import Effect.Aff as Aff
import Data.Foldable (for_)
import Effect.AVar as Async
import Data.Map as Map
import Data.Traversable (for)
import Data.Either (isLeft, fromLeft, Either (..))
import System.Time (getTimestamp)
import Statistics (sendComponentTime) 
import Data.List (head)
import Cache (Cache)

import Undefined

proxy = Proxy :: _ "home"

loc = "TTHouse.Page.Home"

data Action = 
       Initialize 
     | WinResize Int 
     | LangChange String (Array Scaffold.MapPageText)
     | Finalize

type State = 
     { winWidth :: Maybe Int
     , platform :: Maybe Platform
     , body :: String
     , hash :: String
     , start :: Int
     }

component mkBody =
  H.mkComponent
    { initialState: const 
      { winWidth: Nothing
      , platform: Nothing
      , body: mempty
      , hash: mempty
      , start: 0 }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize
      , finalize = pure Finalize
      }
    }
    where 
      render {winWidth: Just w, platform: Just p, body: body} = 
        HH.div_ [mkBody p w (content body) ]
      render _ = HH.div_ []
      handleAction Initialize = do
        H.liftEffect $ window >>= document >>= setTitle "TTH"
        { platform, config: {scaffoldHost: host }, async } <- getStore
        w <- H.liftEffect $ window >>= innerWidth

        tm <- H.liftEffect getTimestamp

        logDebug $ loc <> " component has started at " <> show tm

        void $ initTranslation loc \hash translation ->
          H.modify_ _ { 
              platform = pure platform
           ,  winWidth = pure w
           , body = undefined $ Scaffold.getTranslationPage translation
           , hash = hash
           , start = tm  }

        void $ H.subscribe =<< WinResize.subscribe WinResize

        Meta.set host async $ pure $ Scaffold.MetaPage (show Route.Home)

        Translation.load loc $ \hash translation -> 
          handleAction $ LangChange hash $ Scaffold.getTranslationPage translation

      handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }
      handleAction (LangChange _ _) = undefined 
      handleAction Finalize = do
        end <- H.liftEffect getTimestamp
        {start} <- H.get
        sendComponentTime start end loc

content = HH.text