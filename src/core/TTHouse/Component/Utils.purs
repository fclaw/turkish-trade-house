-- | Some utilities are useful across any component. We'll maintain them in this catch-all module.
module TTHouse.Component.Utils ( OpaqueSlot, withCaptcha ) where

import Prelude

import TTHouse.Api.Foreign.Scaffold as Scaffold
import TTHouse.Api.Foreign.Request as Request
import TTHouse.Capability.LogMessages (logDebug)
import TTHouse.Api.Foreign.Request.Handler (withError)

import Halogen as H
import Data.Function.Uncurried (runFn2)
import Halogen.Store.Monad (getStore)

-- | When a component has no queries or messages, it has no public interface and can be
-- | considered an "opaque" component. The only way for a parent to interact with the
-- | component is by sending input.
type OpaqueSlot slot = forall query. H.Slot query Void slot


withCaptcha true onFailure onSuccess = do 
  { config: {scaffoldHost: host} } <- getStore
  resp <- Request.make host Scaffold.mkReCaptchaApi $ 
    runFn2 Scaffold.goReCaptcha "6Ld138ImAAAAAEB8Ba7V5QTvfFhq433MsF5hZV4v"
  logDebug $ "captcha resp --> " <> show resp
  withError resp \(captcha :: Scaffold.ReCaptcha) -> do 
    let res = Scaffold.getSuccessReCaptcha captcha
    case res of 
      true -> onSuccess
      false -> onFailure
withCaptcha false _ onSuccess = onSuccess