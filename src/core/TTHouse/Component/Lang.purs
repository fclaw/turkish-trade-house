module TTHouse.Component.Lang ( component, proxy ) where

import Prelude

import TTHouse.Component.HTML.Utils (css)
import TTHouse.Capability.LogMessages (logDebug, logError, class LogMessages)
import TTHouse.Capability.Now (class Now)
import TTHouse.Component.Lang.Data
import TTHouse.Api.Foreign.Request as Request
import TTHouse.Api.Foreign.Scaffold as Scaffold
import TTHouse.Data.Route as Route

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
import Data.Traversable (for, sequence)
import Halogen.Store.Monad (getStore, updateStore)
import Effect.AVar as Async
import Data.Map as Map
import Data.Tuple
import Data.List (head)
import Store (Store)
import Data.Enum (toEnum, fromEnum)
import Effect.Aff.Class
import Data.Either (isLeft, blush)
import Data.Array (zip)
import Store (Action (WriteTranslationToCache))
import Effect.AVar as Async
import Effect.Exception (message)

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
        { langVar } <- getStore
        langm <- H.liftEffect $ Async.tryRead langVar
        for_ langm \langXs -> do
          let headm = head $ Map.values langXs
          for_ headm \lang -> H.modify_ _ { lang = fromEnum lang }
      handleAction (Notify idx) = do
        let langm = toEnum idx
        logDebug $ show langm
        for_ langm $ \lang -> do 
           { langVar, config: {scaffoldHost: host}, asyncException} <- getStore
           valm <- H.liftEffect $ Async.tryTake langVar
           res <- for valm \langMap -> do
             let xs = 
                   Map.fromFoldable $ 
                   flip map (fromEnum Home .. fromEnum Menu) \r -> 
                     Tuple (fromMaybe undefined (toEnum r)) lang 
             res <- H.liftEffect $ Async.tryPut xs langVar
             if not res 
             then logError "(TTHouse.Component.Lang) failed to put map into langVar"
             else do 
               logDebug $ show "(TTHouse.Component.Lang) lang change"

               
               void $ H.fork $ cacheTranslation asyncException host $ fromMaybe undefined $ toEnum idx 

               H.modify_ _ { lang = idx }
           when (isNothing res) $ 
             logError "(TTHouse.Component.Lang) var is empty. \
                      \ under the normal circumstences this branch of choice cannot be reached."    
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


cacheTranslation var host lang = do 
  let xs = 
        flip map (fromEnum Route.Home .. fromEnum Route.Service) $
          fromMaybe undefined <<< 
          (toEnum :: Int -> Maybe Route.Route)
  res <- map sequence $ for (map show xs) $
    Request.make host Scaffold.mkFrontApi <<<
    Scaffold.loadTranslation
    Scaffold.Content
    lang  <<<
    Just <<<
    Scaffold.Location
  ifError <- for res \ys -> do 
    let translations = Map.fromFoldable $ zip xs ys
    updateStore $ WriteTranslationToCache translations
  when (isLeft ifError) $ do
    let err = blush ifError
    for_ err $ \e -> do 
      logError $ "async error while trying to cache translation: " <> message e
      let val = { err: e, loc: "TTHouse.Component.Lang:cacheTranslation" }
      updateStore $ WriteTranslationToCache Map.empty
      void $ H.liftEffect $ Async.tryPut val var
