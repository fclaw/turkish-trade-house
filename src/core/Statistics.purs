module Statistics (sendComponentTime) where

import Prelude

import TTHouse.Capability.LogMessages (logDebug)
import TTHouse.Api.Foreign.Request (makeWithResp)
import TTHouse.Api.Foreign.Scaffold as Scaffold

import Effect
import Halogen as H
import Data.Argonaut.Encode.Combinators
import Data.Argonaut.Core (jsonEmptyObject)
import Halogen.Store.Monad (getStore)
import Data.Function.Uncurried (runFn2)
import Foreign (unsafeToForeign)

sendComponentTime start end component = do 
  logDebug "sendComponentTime enter .."
  { config: {scaffoldHost, sha256Commit} } <- getStore
  let payload =  
            "component" := component
         ~> "totalTime" := (end - start)
         ~> jsonEmptyObject
  req <- H.liftEffect $ runFn2 Scaffold.mkLogReq sha256Commit (unsafeToForeign payload)
  void $ makeWithResp scaffoldHost Scaffold.mkFrontApi $ runFn2 Scaffold.sendLog req