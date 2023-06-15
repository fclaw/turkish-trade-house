module TTHouse.Component.HTML.Menu.Navbar ( component, proxy ) where

import Prelude

import TTHouse.Component.HTML.Utils (css)
import TTHouse.Data.Route (Route (..))
import TTHouse.Component.HTML.Menu.Hamburger (mkItem)
import TTHouse.Component.Lang (Lang (..))
import  TTHouse.Capability.LogMessages (logDebug)
import TTHouse.Component.Lang (LangVar (..), Recipients (Navbar))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Data.Array ((..))
import Data.Enum (fromEnum, toEnum)
import DOM.HTML.Indexed.InputType
import Type.Proxy (Proxy(..))
import Halogen.Store.Monad (getStore)
import Concurrent.Channel as Async
import Control.Monad.Rec.Class (forever)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Data.Foldable (for_)
import Effect.AVar as Async

import Undefined


proxy = Proxy :: _ "navbar"

data Action = Initialize | LangChange Lang

type State = { route :: Route, lang :: Lang }

component =
  H.mkComponent
    { initialState: const { route: Home, lang: Eng }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize
      }
    }
    where 
      handleAction Initialize =do 
        { langChannel } <- getStore

        void $ H.fork $ forever $ do
          langm <- H.liftAff do
            Aff.delay $ Milliseconds 500.0
            Async.recv $ _.input langChannel
          for_ langm \x@{ recipients, lang } ->
            case recipients of 
              Navbar -> handleAction $ LangChange lang
              _ -> void $ H.liftAff $ Async.send (_.output langChannel) x
      handleAction (LangChange lang) = do
        H.modify_ _ { lang = lang }
        logDebug $ "(TTHouse.Component.HTML.Menu.Navbar) language change to: " <> show lang

-- taken from: https://codepen.io/albizan/pen/mMWdWZ
render { route } =
  HH.div [css "header-menu-wrapper"]
  [ 
      HH.nav [css "navbar navbar-expand-lg navbar-light"]
      [
          HH.ul [css "navbar-nav"] (map (mkItem route addFontStyle) (fromEnum Home .. fromEnum Service) ) 
      ]
  ]

addFontStyle el = HH.div [HPExt.style "font-size: 20px; text-transform:uppercase;"] [el]