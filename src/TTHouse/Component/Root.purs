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
import TTHouse.Capability.LogMessages (class LogMessages)
import TTHouse.Capability.Now (class Now)
import TTHouse.Component.HTML.Header (header)
import TTHouse.Component.HTML.Body (mkBody)

import Data.Either (hush)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Type.Proxy (Proxy(..))
import Undefined
import Halogen.HTML.Properties as HP

data Query a = Navigate Route a

type State =
  { route :: Maybe Route
  }

data Action
  = Initialize

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , error :: OpaqueSlot Unit
  , about :: OpaqueSlot Unit
  , service :: OpaqueSlot Unit
  )

component
  :: forall m
   . MonadAff m
  => Navigate m
  => LogMessages m
  => Now m
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
    -- first we'll get the route the user landed on
    initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
    -- then we'll navigate to the new route (also setting the hash)
    navigate $ fromMaybe Home initialRoute

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery (Navigate dest a) = do
    H.modify_ _ { route = Just dest }
    pure $ Just a

render :: forall m
  . MonadAff m
  => Navigate m 
  => LogMessages m
  => Now m
  => State
  -> H.ComponentHTML Action ChildSlots m
render { route: Just Home } = HH.slot_ (Proxy :: _ "home") unit (Home.component (mkBody header)) unit
render { route: Just Error } = HH.slot_ (Proxy :: _ "error") unit Error.component unit
render { route: Just About } = HH.slot_ (Proxy :: _ "about") unit (About.component (mkBody header)) unit
render { route: Just Service } = HH.slot_ (Proxy :: _ "service") unit (Service.component (mkBody header)) unit
render _ = HH.div_ [ HH.text "Oh no! That page wasn't found." ]
