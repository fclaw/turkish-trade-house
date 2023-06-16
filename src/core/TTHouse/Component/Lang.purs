module TTHouse.Component.Lang
  ( Lang(..)
  , Recipients(..)
  , component
  , proxy
  )
  where

import Prelude

import TTHouse.Component.HTML.Utils (css)
import  TTHouse.Capability.LogMessages (logDebug, logError)

import Data.Generic.Rep (class Generic)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Data.Enum
import Data.Bounded
import Data.Enum.Generic (genericFromEnum, genericToEnum, genericSucc, genericPred, genericCardinality)
import Data.Array ((..))
import Halogen.HTML.Properties.Extended as HPExt
import Data.Maybe (fromMaybe, Maybe (..), isNothing)
import Undefined
import Halogen.HTML.Events as HE
import Data.Foldable (for_)
import Data.Traversable (for)
import Halogen.Store.Monad (getStore)
import Effect.AVar as Async
import Data.Map as Map
import Data.Tuple


proxy = Proxy :: _ "lang"

data Lang = Eng | Turk

derive instance genericLang :: Generic Lang _
derive instance eqLang :: Eq Lang
derive instance ordLang :: Ord Lang

instance shooLang :: Show Lang where
  show Eng = "English"
  show Turk = "Türkçe"

instance enumLang :: Enum Lang where 
  succ = genericSucc
  pred = genericPred 

instance boundedEnumLang :: BoundedEnum Lang where 
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance boundedLang :: Bounded Lang where 
  top = Turk
  bottom = Eng

data Action = Notify Int

type State = { lang :: Int }

data Recipients = Home | Hamburger | Navbar

derive instance genericRecipients :: Generic Recipients _
derive instance eqRecipients :: Eq Recipients
derive instance ordRecipients :: Ord Recipients

instance enumRecipients :: Enum Recipients where 
  succ = genericSucc
  pred = genericPred 

instance boundedEnumRecipients :: BoundedEnum Recipients where 
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance boundedRecipients :: Bounded Recipients where 
  top = Navbar
  bottom = Home

component =
  H.mkComponent
    { initialState: const { lang: 0 }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where
      handleAction (Notify idx) = do
        let langm = toEnum idx
        logDebug $ show langm
        for_ langm $ \lang -> do 
           {langVar} <- getStore
           valm <- H.liftEffect $ Async.tryTake langVar
           res <- for valm \langMap -> do
             let xs = 
                   Map.fromFoldable $ 
                   flip map (fromEnum Home .. fromEnum Navbar) \r -> 
                     Tuple (fromMaybe undefined (toEnum r)) lang 
             res <- H.liftEffect $ Async.tryPut xs langVar
             if not res 
             then logError "(TTHouse.Component.Lang) failed to put map into langVar"
             else do 
               logDebug $ show "(TTHouse.Component.Lang) lang change"
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