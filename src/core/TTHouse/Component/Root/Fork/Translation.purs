module TTHouse.Component.Root.Fork.Translation (load, init) where

import Prelude

import TTHouse.Api.Foreign.Request as Request
import TTHouse.Api.Foreign.Request.Handler as Request
import TTHouse.Api.Foreign.Scaffold as Scaffold
import TTHouse.Component.Async as Async
import TTHouse.Capability.LogMessages (logDebug, logInfo)

import Halogen as H
import Control.Monad.Rec.Class (forever)
import Effect.Aff as Aff
import Effect.AVar as Async
import Halogen.Store.Monad (getStore, updateStore)
import Data.Traversable (for_)
import Store (Action (WriteTranslationToCache))
import Type.Proxy (Proxy (..))
import Data.Maybe (Maybe (..), isNothing)
import Store (Action (WriteTranslationToCacheV2))
import Crypto.Hash (createHash)

loc = "TTHouse.Component.Root.Fork.Translation:fork"

translation = Proxy :: _ "translation"

load goRootHandle = 
  void $ H.fork $ forever $ do
    H.liftAff $ Aff.delay $ Aff.Milliseconds 500.0
    { langVar, config: {scaffoldHost: host} } <- getStore
    res <- H.liftEffect $ Async.tryRead langVar
    { lang: curr } <- H.get 
    for_ res \income -> 
      when (curr /= income) $ do
        logDebug $ loc <> " ---> " <> show income
        resp <- Request.make host Scaffold.mkFrontApi $ Scaffold.loadTranslationV2 income
        Request.onFailure resp (Async.send <<< flip Async.mkException loc) 
          \translation -> do
           hash <- H.liftEffect $ createHash
           updateStore $ WriteTranslationToCacheV2 translation hash
           logDebug $ loc <> " ---> translation cache has been updated, hash: " <> hash
           goRootHandle income

init = do
 { langVar, config: {scaffoldHost: host} } <- getStore
 res <- H.liftEffect $ Async.tryRead langVar
 for_ res \lang -> do
   resp <- Request.make host Scaffold.mkFrontApi $ Scaffold.loadTranslationV2 lang
   Request.withError resp $ \translation -> do 
     hash <- H.liftEffect $ createHash
     updateStore $ WriteTranslationToCacheV2 translation hash