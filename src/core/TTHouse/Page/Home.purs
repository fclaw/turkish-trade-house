module TTHouse.Page.Home
  ( Action(..)
  , component
  , proxy
  )
  where

import Prelude

import TTHouse.Page.Subscription.WinResize as WinResize 
import TTHouse.Api.Foreign.Scaffold as Scaffold
import TTHouse.Component.Lang (Lang (..))
import  TTHouse.Capability.LogMessages (logDebug, logError)
import TTHouse.Component.Lang (Recipients (Home))
import TTHouse.Api.Foreign.Request as Request

import Halogen as H
import Halogen.HTML as HH
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document, innerWidth)
import Web.HTML (window)
import Type.Proxy (Proxy(..))
import Store (Platform)
import Data.Maybe
import Halogen.Store.Monad (getStore)
import Control.Monad.Rec.Class (forever)
import Effect.Aff as Aff
import Data.Foldable (for_)
import Effect.AVar as Async
import Data.Map as Map
import Data.Traversable (for)
import Data.Either (isLeft, fromLeft, Either (..))
import Unsafe.Coerce

import Undefined

proxy = Proxy :: _ "home"

data Action = Initialize | WinResize Int | LangChange Lang

type State = 
     { winWidth :: Maybe Int
     , platform :: Maybe Platform
     , body :: String
     , lang :: Lang
     }

component mkBody =
  H.mkComponent
    { initialState: const 
      { winWidth: Nothing
      , platform: Nothing
      , body: mempty
      , lang: Eng }
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
        { platform, init, langVar } <- getStore
        w <- H.liftEffect $ window >>= innerWidth
        H.modify_ _ { platform = pure platform, winWidth = pure w, body = Scaffold.getHomeContent init }
        void $ H.subscribe =<< WinResize.subscribe WinResize

        void $ H.fork $ forever $ do
          H.liftAff $ Aff.delay $ Aff.Milliseconds 1000.0
          res <- H.liftEffect $ Async.tryRead langVar
          logDebug $ "(TTHouse.Page.Home) lang map" <> show res  
          for_ res \langMap -> 
            for_ (Map.lookup Home langMap) $ \inlang -> do
              {lang} <- H.get 
              when (inlang /= lang) $
                handleAction $ LangChange inlang

      handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }
      handleAction (LangChange inLang) = do
        logDebug $ "(TTHouse.Page.Home) language change to: " <> show inLang
        { config: {scaffoldHost: host} } <- getStore
        obje <- Request.make host Scaffold.mkFrontApi $ Scaffold.loadTranslation Home inLang
        case obje of 
          Left httpErr ->  logError $ show httpErr
          Right obj -> do 
            respe <- H.liftEffect $ Scaffold.getDataFromResponse obj
            for_ respe \resp -> H.modify_ _ { lang = inLang, body = unsafeCoerce resp }
           
content = HH.text