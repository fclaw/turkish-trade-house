-- | The `Router` component is the root of our Halogen application. Every other component is a
-- | direct descendent of this component. We'll use the router to choose which component to render
-- | given a particular `Route` and to manage the user's location in the application.
-- |
-- | See `Main` to understand how this component is used as the root of the application.
module TTHouse.Component.Root
  ( Action(..)
  , ChildSlots
  , Query(..)
  , State
  , component
  )
  where

import Prelude

import TTHouse.Component.Utils (OpaqueSlot)
import TTHouse.Data.Route (Route(..), routeCodec)
import TTHouse.Page.Home as Home
import TTHouse.Page.Service as Service
import TTHouse.Page.About as About
import TTHouse.Page.Error as Error
import TTHouse.Capability.Navigate
import TTHouse.Capability.LogMessages (class LogMessages, logDebug)
import TTHouse.Capability.Now (class Now)
import TTHouse.Component.HTML.Header as Header
import TTHouse.Component.HTML.Footer as Footer
import TTHouse.Component.HTML.Body as Body
import TTHouse.Component.HTML.Menu.Hamburger as HamburgerMenu
import TTHouse.Component.HTML.Menu.Navbar as NavbarMenu

import Data.Either (hush)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, getStore)
import Halogen.Store.Select (selectEq)
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Type.Proxy (Proxy(..))
import Undefined
import Halogen.HTML.Properties as HP
import Store (Store, printStore, Platform)

data Query a = Navigate Route a

type State = { route :: Maybe Route }

data Action
  = Initialize

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , error :: OpaqueSlot Unit
  , about :: OpaqueSlot Unit
  , service :: OpaqueSlot Unit
  )

component
  :: forall m a
   . MonadAff m
  => Navigate m
  => LogMessages m
  => Now m
  => MonadStore a Store m
  => H.Component Query Unit Void m
component = H.mkComponent
  { initialState: const { route: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction Initialize = do
    store <- getStore
    logDebug $ printStore store
    -- first we'll get the route the user landed on
    initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
    -- then we'll navigate to the new route (also setting the hash)
    navigate $ fromMaybe Home initialRoute
  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery (Navigate dest a) = do
    H.modify_ _ { route = pure dest }
    pure $ Just a

params = 
  { header: Header.html
  , footer: Footer.html
  , hamburger: HamburgerMenu.html
  }

render :: forall m a
  . MonadAff m
  => Navigate m
  => LogMessages m
  => Now m
  => MonadStore a Store m
  => State
  -> H.ComponentHTML Action ChildSlots m
render { route: Just r@Home } = HH.slot_ Home.proxy unit (Home.component (Body.mkBodyHtml params r)) unit
render { route: Just Error } = HH.slot_ Error.proxy unit Error.component unit
render { route: Just r@About } = HH.slot_ About.proxy unit (About.component (Body.mkBodyHtml params r)) unit
render { route: Just r@Service } = HH.slot_ Service.proxy unit (Service.component (Body.mkBodyHtml params r)) unit
render _ = HH.div_ [ HH.text "Oh no! That page wasn't found." ]