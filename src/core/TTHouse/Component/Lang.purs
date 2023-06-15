module TTHouse.Component.Lang
  ( component
  , proxy
  , Lang (..)
  )
  where

import Prelude

import TTHouse.Component.HTML.Utils (css)
import  TTHouse.Capability.LogMessages (logDebug)

import Data.Generic.Rep (class Generic)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Data.Enum
import Data.Bounded
import Data.Enum.Generic (genericFromEnum, genericToEnum, genericSucc, genericPred, genericCardinality)
import Data.Array ((..))
import Halogen.HTML.Properties.Extended as HPExt
import Data.Maybe (fromMaybe, Maybe)
import Undefined
import Halogen.HTML.Events as HE
import Data.Foldable (for_)
import Halogen.Store.Monad (getStore)
import Concurrent.Channel as Async

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
           {langChannel} <- getStore
           isSent <- H.liftAff $ 
             Async.send (_.output langChannel) lang
           logDebug $ show isSent
           H.modify_ _ { lang = idx }
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