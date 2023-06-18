module TTHouse.Api.Foreign.Request (make, makeWithResp) where

import Prelude

import TTHouse.Api.Foreign.Scaffold

import Effect
import Data.Function.Uncurried (runFn1)
import Effect.Exception (Error)
import Effect.Aff.Compat as AC
import Halogen as H
import Data.Either
import Effect.Aff (try)
import Effect.Aff.Class
import Foreign.Object (Object)
import Data.Maybe

makeWithResp
  :: forall m api resp . 
  MonadAff m => 
  String -> 
  (ApiClient -> Effect api) -> 
  (api -> AC.EffectFnAff (Object (Response resp))) -> 
  m (Either Error (Object (Response resp)))
makeWithResp host mkApi runApi = do
  api <- H.liftEffect $ do runFn1 mkApiClient host >>= mkApi
  H.liftAff $ try $ AC.fromEffectFnAff $ runApi api

make
  :: forall m api resp . 
  MonadAff m => 
  String -> 
  (ApiClient -> Effect api) -> 
  (api -> AC.EffectFnAff (Object resp)) -> 
  m (Either Error (Object resp))
make host mkApi runApi = do
  api <- H.liftEffect $ do runFn1 mkApiClient host >>= mkApi
  H.liftAff $ try $ AC.fromEffectFnAff $ runApi api