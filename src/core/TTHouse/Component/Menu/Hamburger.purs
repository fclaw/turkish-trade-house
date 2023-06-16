module TTHouse.Component.Menu.Hamburger ( component, proxy, mkItem ) where

import Prelude

import TTHouse.Component.HTML.Utils (css, safeHref)
import TTHouse.Data.Route (Route (..))
import TTHouse.Component.Lang (Lang (..))
import TTHouse.Capability.LogMessages (logDebug)
import TTHouse.Component.Lang (Recipients (Hamburger))

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

import Undefined


proxy = Proxy :: _ "hamburger"

data Action = Initialize | LangChange Lang

type State = { route :: Route, lang :: Lang }

component =
  H.mkComponent
    { initialState: \r -> { route: r, lang: Eng }
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
            for_ (Map.lookup Hamburger langMap) $ 
              handleAction <<< LangChange

      handleAction (LangChange lang) = do
        H.modify_ _ { lang = lang }
        logDebug $ "(TTHouse.Component.HTML.Menu.Hamburger) language change to: " <> show lang


-- I piggyback on the following implementation https://codepen.io/alvarotrigo/pen/PoJGObg
render { route } =
    HH.div_
    [
        HH.input [HPExt.type_ InputCheckbox, css "toggler"]
    ,   HH.div [css "hamburger"] [HH.div_ []]
    ,   HH.div [css "menu"]
        [
            HH.div [HPExt.style "#position: relative; #top: -50%;margin:0 auto;width:200px"] 
            [HH.ul_ (map (mkItem route addFontStyle) (fromEnum Home .. fromEnum Service) )]
        ]     
    ]
mkItem route applyStyle idx = 
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
    el = applyStyle $ HH.text (show (mkRoute idx))

addFontStyle el = HH.div [HPExt.style "text-transform:uppercase;"] [el]