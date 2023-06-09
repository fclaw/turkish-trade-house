module TTHouse.Api.Foreign.Request (make) where

import Prelude

import TTHouse.Api.Foreign.SendGrid

import Effect
import Data.Function.Uncurried (runFn1)
import Effect.Exception (Error)
import Effect.Aff.Compat as AC
import Halogen as H
import Data.Either
import Effect.Aff (try)
import Effect.Aff.Class
import Foreign
import Data.Maybe

make
  :: forall m api resp . 
  MonadAff m => 
  ApiKey -> 
  (ApiClient -> Effect api) -> 
  (api -> AC.EffectFnAff Foreign) -> 
  m (Either Error Foreign)
make key mkApi runApi = do
 api <- H.liftEffect $ do runFn1 mkApiClient key >>= mkApi
 H.liftAff $ try $ AC.fromEffectFnAff $ runApi api