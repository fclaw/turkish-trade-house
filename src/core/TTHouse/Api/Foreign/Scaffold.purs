-- https://github.com/sendgrid/sendgrid-oai
module TTHouse.Api.Foreign.Scaffold where

import Prelude

import Data.Function.Uncurried (Fn1, Fn2, Fn0)
import Foreign (Foreign)
import Effect (Effect)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Core (Json)
import Data.Either (Either (..))

foreign import data ApiClient :: Type
foreign import data ForeignApi :: Type
foreign import data ScaffoldApiControllerSendGridSendMailRequest :: Type
foreign import data Response :: Type -> Type
foreign import data Error :: Type
foreign import data FrontApi :: Type
foreign import data ScaffoldApiControllerFrontendInitInit :: Type
foreign import data ScaffoldApiControllerFrontendInitContent :: Type

instance showError :: Show Error where
  show = printError

foreign import printError :: Error -> String

instance showScaffoldApiControllerFrontendInitInit :: Show ScaffoldApiControllerFrontendInitInit where
  show = printScaffoldApiControllerFrontendInitInit

foreign import printScaffoldApiControllerFrontendInitInit :: ScaffoldApiControllerFrontendInitInit -> String

foreign import getDataFromResponseImpl 
  :: forall a . 
  (Error -> Either Error a) -> 
  (a -> Either Error a) -> 
  Object (Response a) -> 
  Effect (Either Error a)

getDataFromResponse :: forall a . Object (Response a) -> Effect (Either Error a)
getDataFromResponse = getDataFromResponseImpl Left Right

foreign import mkApiClient :: Fn1 String (Effect ApiClient)

foreign import mkForeignApi :: Fn1 ApiClient (Effect ForeignApi)

type SendGridSendMailRequestBody = { from :: String, personalization :: String, subject :: String, body :: String }

foreign import mkScaffoldApiControllerSendGridSendMailRequest :: Fn1 SendGridSendMailRequestBody (Effect ScaffoldApiControllerSendGridSendMailRequest)

foreign import send :: forall a . Fn2 ScaffoldApiControllerSendGridSendMailRequest ForeignApi (AC.EffectFnAff (Object (Response a)))

foreign import mkFrontApi :: Fn1 ApiClient (Effect FrontApi)

foreign import init :: Fn1 FrontApi  (AC.EffectFnAff (Object (Response ScaffoldApiControllerFrontendInitInit)))

foreign import getHomeContent :: ScaffoldApiControllerFrontendInitInit -> String
foreign import getAboutContent :: ScaffoldApiControllerFrontendInitInit -> String
foreign import getServiceContent :: ScaffoldApiControllerFrontendInitInit -> String
foreign import getShaCommit :: ScaffoldApiControllerFrontendInitInit -> String

