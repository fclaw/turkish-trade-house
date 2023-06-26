-- https://github.com/sendgrid/sendgrid-oai
module TTHouse.Api.Foreign.Scaffold where

import Prelude

import TTHouse.Component.Lang.Data (Lang)
import TTHouse.Data.Route (Route)

import Data.Function.Uncurried (Fn1, Fn2, Fn0, Fn4, runFn4, runFn2)
import Effect (Effect)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Core (Json)
import Data.Either (Either (..))
import Data.Maybe (Maybe, fromMaybe)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Encode.Combinators
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode.Encoders (encodeMaybe)
import Foreign (Foreign)
import Effect.Exception as E
import Data.Tuple

import Undefined


foreign import data ApiClient :: Type
foreign import data ForeignApi :: Type
foreign import data SendGridSendMailRequest :: Type
foreign import data Response :: Type -> Type
foreign import data Error :: Type
foreign import data FrontApi :: Type
foreign import data Init :: Type
foreign import data Content :: Type
foreign import data ResponseTranslation :: Type
foreign import data Translation :: Type
foreign import data MenuItemObj :: Type
foreign import data FrontendLogRequest :: Type
foreign import data Cookie :: Type
foreign import data ResponseCookie :: Type
foreign import data ResponseMeta :: Type
foreign import data Meta :: Type
foreign import data ReCaptchaApi :: Type
foreign import data ResponseReCaptcha :: Type
foreign import data ReCaptcha :: Type
foreign import data MapMenuText :: Type

instance showError :: Show Error where
  show = printError

foreign import printError :: Error -> String

instance Show Init where
  show = printFrontendInit


instance Show Translation where 
  show = getTranslatedContent


foreign import printFrontendInit :: Init -> String

foreign import getDataFromResponseImpl 
  :: forall a . 
  (String -> Either E.Error a) -> 
  (a -> Either E.Error a) -> 
  Object (Response a) -> 
  Either E.Error a

getDataFromResponse :: forall a . Object (Response a) -> Either E.Error a
getDataFromResponse = getDataFromResponseImpl (Left <<< E.error) Right

foreign import getDataFromObjImpl 
  :: forall a b . 
  (String -> Either E.Error b) -> 
  (b -> Either E.Error b) -> 
  Object a -> 
  Effect (Either E.Error b)

getDataFromObj :: forall a b . Object a -> Effect (Either E.Error b)
getDataFromObj = getDataFromObjImpl (Left <<< E.error) Right

foreign import mkApiClient :: Fn1 String (Effect ApiClient)

foreign import mkForeignApi :: Fn1 ApiClient (Effect ForeignApi)

type SendGridSendMailRequestBody = { from :: String, personalization :: String, subject :: String, body :: String }

foreign import mkSendGridSendMailRequest :: Fn1 SendGridSendMailRequestBody (Effect SendGridSendMailRequest)

foreign import send :: forall a . Fn2 SendGridSendMailRequest ForeignApi (AC.EffectFnAff (Object (Response a)))

foreign import mkFrontApi :: Fn1 ApiClient (Effect FrontApi)

foreign import init :: Fn1 FrontApi  (AC.EffectFnAff (Object (Response Init)))

foreign import getHomeContent :: Init -> String
foreign import getAboutContent :: Init -> String
foreign import getServiceContent :: Init -> String
foreign import getShaCommit :: Init -> String
foreign import getShaCSSCommit :: Init -> String
foreign import getCookiesInit :: Init -> Array String

foreign import loadTranslationImpl :: Fn4 Json Json Json FrontApi (AC.EffectFnAff (Object ResponseTranslation))

foreign import loadTranslationImplV2 :: Fn2 Json FrontApi (AC.EffectFnAff (Object ResponseTranslation))

foreign import getTranslationMenu :: Translation -> Array MapMenuText

foreign import _showMapMenuText :: MapMenuText -> String

instance Show MapMenuText where
  show = _showMapMenuText


type Tpl = { key :: String, value :: String }

foreign import getKeyMenuText :: MapMenuText -> String
foreign import getValMenuText :: MapMenuText -> String

transformMenuTextToTpl :: MapMenuText -> Tuple String String
transformMenuTextToTpl x = Tuple (getKeyMenuText x) (getValMenuText x) 

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

loadTranslationV2 ::Lang -> FrontApi -> (AC.EffectFnAff (Object ResponseTranslation))
loadTranslationV2 lang = runFn2 loadTranslationImplV2 (encodeJson lang)


foreign import  getTranslatedContent :: Translation -> String

foreign import getTranslatedMenuArray :: Translation -> Array MenuItemObj

foreign import getMenuItemKey :: MenuItemObj -> String
foreign import getMenuItemVal :: MenuItemObj -> String


foreign import mkLogReq :: Fn2 String Foreign (Effect FrontendLogRequest)

foreign import sendLog :: Fn2 FrontendLogRequest FrontApi (AC.EffectFnAff (Object (Response Unit)))


instance Show Cookie where 
  show = showCookieImpl

foreign import showCookieImpl :: Cookie -> String

foreign import getCookies :: Fn1 FrontApi (AC.EffectFnAff (Object ResponseCookie))

newtype MetaPage = MetaPage String

instance EncodeJson MetaPage where
  encodeJson (MetaPage page) = "page" := page ~> jsonEmptyObject

foreign import getMetaImpl :: Fn2 Json FrontApi (AC.EffectFnAff (Object ResponseMeta))

getMeta :: Maybe MetaPage -> FrontApi -> AC.EffectFnAff (Object ResponseMeta)
getMeta page = runFn2 getMetaImpl (encodeJson (fromMaybe undefined page))

foreign import getMetaDescription :: Meta -> String


foreign import mkReCaptchaApi :: Fn1 ApiClient (Effect ReCaptchaApi)

foreign import goReCaptcha :: Fn2 String ReCaptchaApi (AC.EffectFnAff (Object (ResponseReCaptcha)))

foreign import getSuccessReCaptcha :: ReCaptcha -> Boolean

instance Show ReCaptcha where 
  show x = "{ success: " <> show (getSuccessReCaptcha x) <> " }"
