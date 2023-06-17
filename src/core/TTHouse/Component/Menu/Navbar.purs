module TTHouse.Component.Menu.Navbar ( component, proxy ) where

import Prelude

import TTHouse.Component.HTML.Utils (css)
import TTHouse.Data.Route (Route (..))
import TTHouse.Component.Menu.Hamburger (mkItem, getMenuByLang)
import TTHouse.Component.Lang (Lang (..))
import TTHouse.Capability.LogMessages (logDebug, logError)
import TTHouse.Component.Lang (Recipients (Menu))
import TTHouse.Locale as Locale

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Data.Array ((..))
import Data.Enum (fromEnum, toEnum)
import DOM.HTML.Indexed.InputType
import Type.Proxy (Proxy(..))
import Halogen.Store.Monad (getStore)
import Control.Monad.Rec.Class (forever)
import Effect.Aff as Aff
import Data.Foldable (for_)
import Effect.AVar as Async
import Data.Map as Map
import Data.Traversable (for)
import Data.Maybe (Maybe (..), isNothing)

import Undefined


proxy = Proxy :: _ "navbar"

data Action = Initialize | LangChange Lang

type State = 
     { route :: Route
     , lang :: Lang 
     , routesTitle :: Map.Map Route String  
     }

component =
  H.mkComponent
    { initialState: \r -> { route: r, lang: Eng, routesTitle: Map.empty }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize
      }
    }
    where 
      handleAction Initialize = do
        { langVar } <- getStore

        void $ H.fork $ forever $ do
          H.liftAff $ Aff.delay $ Aff.Milliseconds 1000.0
          res <- H.liftEffect $ Async.tryRead langVar
          for_ res \langMap -> 
            for_ (Map.lookup Menu langMap) $ 
              handleAction <<< LangChange

      handleAction (LangChange lang) = do
       xsm <- getMenuByLang lang
       res <- for xsm \xs -> do 
          H.modify_ _ { lang = lang, routesTitle = xs }
          logDebug $ "(TTHouse.Component.HTML.Menu.Navbar) language change to: " <> show lang
          pure $ Just unit
       when (isNothing res) $ logError "locale connot be changed" 

-- taken from: https://codepen.io/albizan/pen/mMWdWZ
render { route, routesTitle } =
  HH.div [css "header-menu-wrapper"]
  [ 
      HH.nav [css "navbar navbar-expand-lg navbar-light"]
      [
          HH.ul [css "navbar-nav"] (map (mkItem route routesTitle addFontStyle) (fromEnum Home .. fromEnum Service) ) 
      ]
  ]

addFontStyle el = HH.div [HPExt.style "font-size: 20px; text-transform:uppercase;"] [el]