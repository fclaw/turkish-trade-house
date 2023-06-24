module TTHouse.Document.Meta (set) where

import Prelude

import TTHouse.Api.Foreign.Scaffold as Scaffold
import TTHouse.Api.Foreign.Request as Request
import TTHouse.Api.Foreign.Request.Handler (onFailure)
import TTHouse.Capability.LogMessages (logWarn)
import TTHouse.Component.Async as Async

import Halogen as H
import Data.Maybe (Maybe (Just))
import Effect.AVar as Async
import Effect.Exception (message)
import Web.DOM.Document (createElement, getElementsByTagName)
import Web.DOM.HTMLCollection (item)
import Web.DOM.Element (setAttribute)
import Web.DOM.Node (appendChild)
import Data.Traversable (for)
import Web.HTML.HTMLDocument (toDocument, toNode)
import Web.DOM.Internal.Types (Element)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.Window (document)
import Web.HTML (window)
import Data.Maybe (isNothing)

set host var page = 
  void $ H.fork $ do 
    resp <- Request.make host Scaffold.mkFrontApi $ Scaffold.getMeta page
    onFailure resp showWarn \(x :: Scaffold.Meta) -> do
      res <- H.liftEffect do 
        let descrip = Scaffold.getMetaDescription x
        win <- window
        doc <- map toDocument $ document win
        xs <- "head" `getElementsByTagName` doc
        headm <- 0 `item` xs
        for headm \(head :: Element) -> do
          meta :: Element <- "meta" `createElement` doc
          setAttribute "name" "description" meta
          setAttribute "content" descrip meta
          appendChild (toNode (unsafeCoerce meta)) (toNode (unsafeCoerce head))
          pure $ Just unit
      when (isNothing res) shoWWarnAppendFailure
  where
    showWarn e = do 
      logWarn $ "TTHouse.Document.Meta: " <> message e
      Async.send $ Async.mkOrdinary "meta cannot be loaded for this page" Async.Warning $ Just "TTHouse.Document.Meta"
    shoWWarnAppendFailure = do 
      logWarn $ "TTHouse.Document.Meta: meta cannot be latched to head"
      Async.send $ Async.mkOrdinary "meta cannot be latched to head" Async.Warning $ Just "TTHouse.Document.Meta"