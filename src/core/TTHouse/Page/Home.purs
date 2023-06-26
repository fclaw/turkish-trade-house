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
import Cache (readTranslation, Cache)

proxy = Proxy :: _ "home"

componentName = "TTHouse.Page.Home"

data Action = 
       Initialize 
     | WinResize Int 
     | LangChange Lang 
     | Finalize

type State = 
     { winWidth :: Maybe Int
     , platform :: Maybe Platform
     , body :: String
     , lang :: Lang
     , start :: Int
     }

component mkBody =
  H.mkComponent
    { initialState: const 
      { winWidth: Nothing
      , platform: Nothing
      , body: mempty
      , lang: Eng
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
        { platform, init, langVar_, cache, config: {scaffoldHost: host}, async } <- getStore
        w <- H.liftEffect $ window >>= innerWidth

        tm <- H.liftEffect getTimestamp

        logDebug $ "(" <> componentName <> ") component has started at " <> show tm

        let content = setContent cache $ Scaffold.getHomeContent init

        H.modify_ _ { 
            platform = pure platform
          , winWidth = pure w
          , body = content
          , start = tm  }

        void $ H.subscribe =<< WinResize.subscribe WinResize

        void $ H.fork $ forever $ do
          H.liftAff $ Aff.delay $ Aff.Milliseconds 500.0
          res <- H.liftEffect $ Async.tryRead langVar_
          logDebug $ "(TTHouse.Page.Home) lang map" <> show res  
          for_ res \langMap -> 
            for_ (Map.lookup Home langMap) $ \inlang -> do
              {lang} <- H.get 
              let headm = head $ Map.values langMap
              for_ headm \x -> 
                when (x /= lang) $
                  for_ (Map.lookup Home langMap) $ 
                    handleAction <<< LangChange

        Meta.set host async $ pure $ Scaffold.MetaPage (show Route.Home)

      handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }
      handleAction (LangChange inLang) = do
        logDebug $ "(TTHouse.Page.Home) language change to: " <> show inLang
        { config: {scaffoldHost: host} } <- getStore
        resp <- Request.make host Scaffold.mkFrontApi $ 
                  Scaffold.loadTranslation 
                  Scaffold.Content
                  inLang 
                  (Just (Scaffold.Location "home") )
        withError resp \x -> H.modify_ _ { lang = inLang, body = Scaffold.getTranslatedContent x }
      handleAction Finalize = do 
        end <- H.liftEffect getTimestamp
        {start} <- H.get
        sendComponentTime start end componentName

content = HH.text


setContent :: Cache -> String -> String
setContent cache = flip fromMaybe (readTranslation Route.Home cache)