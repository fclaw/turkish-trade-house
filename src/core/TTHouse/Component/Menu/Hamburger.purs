module TTHouse.Component.Menu.Hamburger ( component, proxy, mkItem, getMenuByLang ) where

import Prelude

import TTHouse.Component.HTML.Utils (css, safeHref)
import TTHouse.Data.Route (Route (..))
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
import Data.Maybe
import Data.Array.NonEmpty (fromArray)
import Data.Traversable (for)
import Data.Map as Map

import Undefined


proxy = Proxy :: _ "hamburger"

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
          logDebug $ "(TTHouse.Component.HTML.Menu.Hamburger) language change to: " <> show lang
          pure $ Just unit
        when (isNothing res) $ logError "locale connot be changed"  

-- I piggyback on the following implementation https://codepen.io/alvarotrigo/pen/PoJGObg
render { route, routesTitle } =
    HH.div_
    [
        HH.input [HPExt.type_ InputCheckbox, css "toggler"]
    ,   HH.div [css "hamburger"] [HH.div_ []]
    ,   HH.div [css "menu"]
        [
            HH.div [HPExt.style "#position: relative; #top: -50%;margin:0 auto;width:200px"] 
            [HH.ul_ (map (mkItem route routesTitle addFontStyle) (fromEnum Home .. fromEnum Service) )]
        ]     
    ]
mkItem route xs applyStyle idx = 
  HH.li_ 
  [
      HH.a 
      [ css "nav-link"
      , safeHref (mkRoute idx)
      , isDisabled (mkRoute idx == route)
      ] 
      [el] 
  ] 
  where 
    mkRoute = fromMaybe undefined <<< (toEnum :: Int -> Maybe Route)
    isDisabled true = HPExt.style "cursor: not-allowed;"
    isDisabled false = HPExt.style mempty
    title = fromMaybe (show (mkRoute idx)) $ Map.lookup (mkRoute idx) xs
    el = applyStyle $ HH.text title

addFontStyle el = HH.div [HPExt.style "text-transform:uppercase;"] [el]


getMenuByLang lang = do
  let fromIdx = fromMaybe undefined <<< (toEnum :: Int -> Maybe Route)
  let xsm = fromArray $ map fromIdx (fromEnum Home .. fromEnum Service)
  for xsm \xs -> H.liftEffect $ Locale.getMap xs lang
