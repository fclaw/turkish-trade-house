module TTHouse.Component.Menu.Navbar ( component, proxy ) where

import Prelude

import TTHouse.Component.HTML.Utils (css)
import TTHouse.Data.Route (Route (..))
import TTHouse.Component.Menu.Hamburger (mkItem, getMenuByLang)
import TTHouse.Component.Lang.Data (Lang (..))
import TTHouse.Capability.LogMessages (logDebug, logError)
import TTHouse.Component.Lang.Data (Recipients (Menu))

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
import Data.Traversable (for)
import Data.Maybe (Maybe (..), isNothing)
import Data.Either (Either (..), isLeft, fromLeft)
import Data.List (head)
import Store (Action (WriteMenuToCache))
import Cache as Cache

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
    { initialState: identity
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize
      }
    }
    where 
      handleAction Initialize = do
        { langVar, cache } <- getStore

        H.modify_ _ { routesTitle = Cache.readMenu cache }
 
        void $ H.fork $ forever $ do
          H.liftAff $ Aff.delay $ Aff.Milliseconds 500.0
          { lang } <- H.get 
          res <- H.liftEffect $ Async.tryRead langVar
          for_ res \langMap -> do
            let headm = head $ Map.values langMap
            for_ headm \x -> 
              if x /= lang then
                for_ (Map.lookup Menu langMap) $ 
                  handleAction <<< LangChange
              else logDebug "(TTHouse.Component.HTML.Menu.Navbar) menu is up-to-date. skip."

      handleAction (LangChange lang) = do
       { config: {scaffoldHost: host} } <- getStore
       xse <- getMenuByLang host lang
       case xse of 
         Right xs -> do 
           H.modify_ _ { lang = lang, routesTitle = xs }
           updateStore $ WriteMenuToCache xs
           logDebug $ "(TTHouse.Component.HTML.Menu.Navbar) language change to: " <> show lang
         Left err -> logError $ show err

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