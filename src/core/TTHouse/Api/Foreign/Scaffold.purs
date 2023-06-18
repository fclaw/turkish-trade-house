-- https://github.com/sendgrid/sendgrid-oai
module TTHouse.Api.Foreign.Scaffold where

import Prelude

import TTHouse.Component.Lang.Data (Lang)
import TTHouse.Data.Route (Route)

import Data.Function.Uncurried (Fn1, Fn2, Fn0, Fn4, runFn4)
import Effect (Effect)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Core (Json)
import Data.Either (Either (..))
import Data.Maybe (Maybe)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Encode.Combinators
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode.Encoders (encodeMaybe)


foreign import data ApiClient :: Type
foreign import data ForeignApi :: Type
foreign import data ScaffoldApiControllerSendGridSendMailRequest :: Type
foreign import data Response :: Type -> Type
foreign import data Error :: Type
foreign import data FrontApi :: Type
foreign import data ScaffoldApiControllerFrontendInitInit :: Type
foreign import data ScaffoldApiControllerFrontendInitContent :: Type
foreign import data ResponseTranslation :: Type
foreign import data Translation :: Type
foreign import data ScaffoldApiControllerFrontendTranslateMenuItemObj :: Type

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

foreign import getDataFromObjImpl 
  :: forall a b . 
  (Error -> Either Error b) -> 
  (b -> Either Error b) -> 
  Object a -> 
  Effect (Either Error b)

getDataFromObj :: forall a b . Object a -> Effect (Either Error b)
getDataFromObj = getDataFromObjImpl Left Right

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
foreign import getShaCSSCommit :: ScaffoldApiControllerFrontendInitInit -> String

foreign import loadTranslationImpl :: Fn4 Json Json Json FrontApi (AC.EffectFnAff (Object ResponseTranslation))


data Resource = Content | Menu

instance showResource :: Show Resource where
  show Content = "home"
  show Menu = "menu"

instance encodeJsonResource :: EncodeJson Resource where
  encodeJson Content = encodeJson "content"
  encodeJson Menu = encodeJson "menu"


newtype Location = Location String

instance encodeJsonLocation :: EncodeJson Location where
  encodeJson (Location location) = "location" := location ~> jsonEmptyObject

loadTranslation :: Resource -> Lang -> Maybe Location -> FrontApi -> (AC.EffectFnAff (Object ResponseTranslation))
loadTranslation resource lang loc = runFn4 loadTranslationImpl (encodeJson resource) (encodeJson lang) (encodeMaybe encodeJson loc)

foreign import  getTranslatedContent :: Translation -> String

foreign import getTranslatedMenuArray :: Translation -> Array ScaffoldApiControllerFrontendTranslateMenuItemObj

foreign import getMenuItemKey :: ScaffoldApiControllerFrontendTranslateMenuItemObj -> String
foreign import getMenuItemVal :: ScaffoldApiControllerFrontendTranslateMenuItemObj -> String

