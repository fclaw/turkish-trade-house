-- https://github.com/sendgrid/sendgrid-oai
module TTHouse.Api.Foreign.Scaffold where

import Prelude

import Data.Function.Uncurried (Fn1, Fn2)
import Foreign (Foreign)
import Effect (Effect)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Core (Json)
import Data.Either (Either (..))

foreign import data ApiClient :: Type
foreign import data SendGridApi :: Type
foreign import data ScaffoldApiControllerSendGridSendMailRequest :: Type
foreign import data Response :: Type -> Type
foreign import data Error :: Type

instance showError :: Show Error where 
  show = printError

foreign import printError :: Error -> String

foreign import mkApiClient :: Fn1 String (Effect ApiClient)

type Subject = String 
type Content = String

foreign import mkSendGridApi :: Fn1 ApiClient (Effect SendGridApi)

type SendGridSendMailRequestBody = { from :: String, personalization :: String, subject :: String, body :: String }

foreign import mkScaffoldApiControllerSendGridSendMailRequest :: Fn1 SendGridSendMailRequestBody (Effect ScaffoldApiControllerSendGridSendMailRequest)

foreign import send :: forall a . Fn2 ScaffoldApiControllerSendGridSendMailRequest SendGridApi (AC.EffectFnAff (Object (Response a)))

foreign import getDataFromResponseImpl 
  :: forall a . 
  (Error -> Either Error a) -> 
  (a -> Either Error a) -> 
  Object (Response a) -> 
  Effect (Either Error a)

getDataFromResponse :: forall a . Object (Response a) -> Effect (Either Error a)
getDataFromResponse = getDataFromResponseImpl Left Right