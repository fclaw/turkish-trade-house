module TTHouse.Component.Menu.Hamburger ( component, proxy ) where

import Prelude

import TTHouse.Component.HTML.Utils (css, safeHref)
import TTHouse.Data.Route (Route (..))
import TTHouse.Component.Lang.Data (Lang (..))
import TTHouse.Capability.LogMessages (logDebug, logError)
import TTHouse.Component.Lang.Data (Recipients (Menu))
import TTHouse.Api.Foreign.Scaffold as Scaffold
import TTHouse.Component.Subscription.Translation as Translation
import TTHouse.Component.Utils (initTranslation)
import TTHouse.Component.Menu.Navbar ( mkItem, transformToMenu )

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Data.Array ((..))
import Data.Enum (fromEnum, toEnum)
import DOM.HTML.Indexed.InputType
import Type.Proxy (Proxy(..))
import Halogen.Store.Monad (getStore, updateStore)
import Control.Monad.Rec.Class (forever)
import Effect.Aff as Aff
import Data.Foldable (for_)
import Effect.AVar as Async
import Data.Map as Map
import Data.Maybe
import Data.Array.NonEmpty (fromArray)
import Data.Traversable (for, traverse)
import Data.Either (Either (..), isLeft, fromLeft)
import Control.Monad.Except.Trans (runExceptT, except, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap, lmap)
import Foreign (readArray)
import Data.Tuple (Tuple (..))
import Data.List (head)
import Store (Action (WriteMenuToCache))
import Cache as Cache

import Undefined


proxy = Proxy :: _ "hamburger"

loc = "TTHouse.Component.HTML.Menu.Hamburger"

data Action = Initialize | LangChange String (Array Scaffold.MapMenuText)

type State = 
     { route :: Route
     , menu :: Map.Map String String
     , hash :: String
     }

component =
  H.mkComponent
    { initialState:
      \{ lang, route } -> { route: route, menu: Map.empty, hash: mempty }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize
      }
    }
    where 
      handleAction Initialize = do
        void $ initTranslation loc \hash translation -> 
          H.modify_ _ { 
              hash = hash
            , menu = 
              transformToMenu $ 
              Scaffold.getTranslationMenu translation }
        { menu, hash } <- H.get
        logDebug $ loc <> " ---> " <> show (Map.keys menu)
        logDebug $ loc <> " hash: ---> " <> hash
        Translation.load loc $ \hash translation -> 
          handleAction $ LangChange hash $ Scaffold.getTranslationMenu translation
      handleAction (LangChange hash xs) = do 
        logDebug $ loc <> " ---> " <> show xs
        logDebug $ loc <> " hash: ---> " <> hash
        H.modify_ _ { hash = hash, menu = transformToMenu xs }
     

-- I piggyback on the following implementation https://codepen.io/alvarotrigo/pen/PoJGObg
render { route, menu } =
    HH.div_
    [
        HH.input [HPExt.type_ InputCheckbox, css "toggler"]
    ,   HH.div [css "hamburger"] [HH.div_ []]
    ,   HH.div [css "menu"]
        [
            HH.div [HPExt.style "#position: relative; #top: -50%;margin:0 auto;width:200px"] 
            [HH.ul_ (map (mkItem route menu addFontStyle) (fromEnum Home .. fromEnum Service) )]
        ]     
    ]

addFontStyle el = HH.div [HPExt.style "text-transform:uppercase;"] [el]