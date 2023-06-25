module TTHouse.Component.Lang ( component, proxy ) where

import Prelude

import TTHouse.Component.HTML.Utils (css)
import TTHouse.Capability.LogMessages (logDebug, class LogMessages)
import TTHouse.Capability.Now (class Now)
import TTHouse.Component.Lang.Data
import TTHouse.Data.Route as Route
import TTHouse.Component.Async as Async

import Halogen.Store.Monad (class MonadStore)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Data.Array ((..))
import Halogen.HTML.Properties.Extended as HPExt
import Data.Maybe (fromMaybe, Maybe (..), isNothing)
import Undefined
import Halogen.HTML.Events as HE
import Data.Foldable (for_)
import Halogen.Store.Monad (getStore)
import Store (Store)
import Data.Enum (toEnum, fromEnum)
import Effect.Aff.Class
import Store (Action)
import Effect.AVar as Async

proxy = Proxy :: _ "lang"

component :: forall q i o m . MonadStore Action Store m => MonadAff m => LogMessages m => Now m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const { lang: 0 }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize }
    }
    where
      handleAction Initialize = do
        { lang } <- getStore
        langm <- H.liftEffect $ Async.tryRead lang
        for_ langm \currLang -> H.modify_ _ { lang = fromEnum currLang }
      handleAction (Notify idx) = do
        let langm = toEnum idx
        logDebug $ show langm
        for_ langm $ \inLang -> do 
           { lang } <- getStore
           void $ H.liftEffect $ Async.tryPut inLang lang 
render {lang} = 
  HH.div [css "header-lang-wrapper"] 
  [
      HH.select 
      [ css "form-select form-select-sm"
      , HE.onSelectedIndexChange Notify
      , HPExt.selectedIndex lang
      ]
      (flip map (fromEnum Eng .. fromEnum Turk) $ \ident ->
         let str = show <<< fromMaybe undefined <<< (toEnum :: Int -> Maybe Lang)
         in HH.option [HPExt.value (str ident)] [HH.text (str ident)])
  ]