module TTHouse.Api.Scaffold.FrontApi
  ( Cookie
  , FrontApi
  , FrontendLogRequest
  , Init
  , MapMenuText
  , MapPageText
  , Meta
  , MetaPage(..)
  , ResponseCookie
  , ResponseMeta
  , ResponseTranslation
  , Translation
  , getAboutContent
  , getCookies
  , getCookiesInit
  , getHomeContent
  , getKeyMenuText
  , getMeta
  , getMetaDescription
  , getServiceContent
  , getShaCSSCommit
  , getShaCommit
  , getTranslationMenu
  , getTranslationPage
  , getValMenuText
  , init
  , loadTranslation
  , mkFrontApi
  , mkLogReq
  , sendLog
  , transformMenuTextToTpl
  )
  where

import Prelude

import TTHouse.Api.Foreign.Common
import TTHouse.Component.Lang.Data (Lang)

import Data.Function.Uncurried (Fn2, Fn1, runFn2)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Data.Argonaut.Encode (encodeJson, class EncodeJson)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Combinators
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe, fromMaybe)
import Undefined
import Foreign (Foreign)
import Effect (Effect)

foreign import data MapMenuText :: Type
foreign import data MapPageText :: Type
foreign import data FrontApi :: Type
foreign import data Translation :: Type
foreign import data FrontendLogRequest :: Type
foreign import data Cookie :: Type
foreign import data ResponseCookie :: Type
foreign import data ResponseMeta :: Type
foreign import data Meta :: Type
foreign import data Init :: Type
foreign import data ResponseTranslation :: Type

foreign import mkFrontApi :: Fn1 ApiClient (Effect FrontApi)

foreign import init :: Fn1 FrontApi  (AC.EffectFnAff (Object (Response Init)))

instance Show Init where
  show = _showInit

foreign import _showInit :: Init -> String

foreign import getHomeContent :: Init -> String
foreign import getAboutContent :: Init -> String
foreign import getServiceContent :: Init -> String
foreign import getShaCommit :: Init -> String
foreign import getShaCSSCommit :: Init -> String
foreign import getCookiesInit :: Init -> Array String

foreign import _loadTranslation :: Fn2 Json FrontApi (AC.EffectFnAff (Object ResponseTranslation))

foreign import mkLogReq :: Fn2 String Foreign (Effect FrontendLogRequest)

foreign import sendLog :: Fn2 FrontendLogRequest FrontApi (AC.EffectFnAff (Object (Response Unit)))

instance Show Cookie where 
  show = _showCookie

foreign import _showCookie :: Cookie -> String

foreign import getCookies :: Fn1 FrontApi (AC.EffectFnAff (Object ResponseCookie))

newtype MetaPage = MetaPage String

instance EncodeJson MetaPage where
  encodeJson (MetaPage page) = "page" := page ~> jsonEmptyObject

foreign import _getMeta :: Fn2 Json FrontApi (AC.EffectFnAff (Object ResponseMeta))

getMeta :: Maybe MetaPage -> FrontApi -> AC.EffectFnAff (Object ResponseMeta)
getMeta page = runFn2 _getMeta (encodeJson (fromMaybe undefined page))

foreign import getMetaDescription :: Meta -> String

loadTranslation :: Lang -> FrontApi -> (AC.EffectFnAff (Object ResponseTranslation))
loadTranslation lang = runFn2 _loadTranslation (encodeJson lang)

foreign import getTranslationMenu :: Translation -> Array MapMenuText
foreign import getTranslationPage :: Translation -> Array MapPageText

foreign import getKeyMenuText :: MapMenuText -> String
foreign import getValMenuText :: MapMenuText -> String

foreign import _showMapMenuText :: MapMenuText -> String

instance Show MapMenuText where
  show = _showMapMenuText

transformMenuTextToTpl :: MapMenuText -> Tuple String String
transformMenuTextToTpl x = Tuple (getKeyMenuText x) (getValMenuText x) 
