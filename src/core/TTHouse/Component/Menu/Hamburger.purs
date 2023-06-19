module TTHouse.Component.Menu.Hamburger ( component, proxy, mkItem, getMenuByLang ) where

import Prelude

import TTHouse.Component.HTML.Utils (css, safeHref)
import TTHouse.Data.Route (Route (..))
import TTHouse.Component.Lang.Data (Lang (..))
import TTHouse.Capability.LogMessages (logDebug, logError)
import TTHouse.Component.Lang.Data (Recipients (Menu))
import TTHouse.Api.Foreign.Scaffold as Scaffold
import TTHouse.Api.Foreign.Request as Request

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

data Action = Initialize | LangChange Lang

type State = 
     { route :: Route
     , lang :: Lang
     , routesTitle :: Map.Map String String 
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
              when (x /= lang) $ 
                for_ (Map.lookup Menu langMap) $ 
                  handleAction <<< LangChange

      handleAction (LangChange lang) = do
        { config: {scaffoldHost: host} } <- getStore
        xse <- getMenuByLang host lang
        res <- for xse \xs -> do 
          H.modify_ _ { lang = lang, routesTitle = xs }
          updateStore $ WriteMenuToCache xs
          logDebug $ "(TTHouse.Component.HTML.Menu.Hamburger) language change to: " <> show lang
        when (isLeft res) $ logError $ show $ fromLeft "cannot read left" res
       
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
    title = fromMaybe (show (mkRoute idx)) $ Map.lookup (show (mkRoute idx)) xs
    el = applyStyle $ HH.text title

addFontStyle el = HH.div [HPExt.style "text-transform:uppercase;"] [el]


getMenuByLang host lang = runExceptT $ do 
  resp <- lift $ Request.make host Scaffold.mkFrontApi $ Scaffold.loadTranslation Scaffold.Menu lang Nothing
  xs <- except $ bimap show Scaffold.getTranslatedMenuArray resp
  pure $ Map.fromFoldable $ flip map xs $ \el -> 
    Tuple (Scaffold.getMenuItemKey el) (Scaffold.getMenuItemVal el)


