module TTHouse.Component.Root.Fork.Translation (load) where

import Prelude

import TTHouse.Api.Foreign.Request as Request
import TTHouse.Api.Foreign.Request.Handler as Request
import TTHouse.Api.Foreign.Scaffold as Scaffold
import TTHouse.Component.Async as Async

import Halogen as H
import Control.Monad.Rec.Class (forever)
import Effect.Aff as Aff
import Effect.AVar as Async
import Halogen.Store.Monad (getStore, updateStore)
import Data.Traversable (for_)
import Store (Action (WriteTranslationToCache))
import Type.Proxy (Proxy (..))

loc = "TTHouse.Component.Root.Fork.Translation:fork"

translation = Proxy :: _ "translation"

load = 
  void $ H.fork $ forever $ do
    H.liftAff $ Aff.delay $ Aff.Milliseconds 500.0
    { lang, config: {scaffoldHost: host} } <- getStore
    langm <- H.liftEffect $ Async.tryTake lang
    for_ langm \x -> do
      resp <- Request.make host Scaffold.mkFrontApi $ Scaffold.loadTranslationV2 x
      Request.onFailure resp (Async.send <<< flip Async.mkException loc) (updateStore <<< WriteTranslationToCache)