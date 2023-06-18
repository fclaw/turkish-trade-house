-- | A global state containing information useful to most components in the
-- | application. If you've ever used Redux, this should look familiar.
-- | Components can read, write, and subscribe to this central state, which is
-- | called a "store" by convention.
module Store
  ( Action(..)
  , Platform(..)
  , Store(..)
  , initAppStore
  , printStore
  , readPlatform
  , reduce
  )
  where

import Prelude

import TTHouse.Data.Config (Config)
import TTHouse.Api.Foreign.Scaffold as Scaffold 
import TTHouse.Capability.LogMessages (logError, logDebug)
import TTHouse.Api.Foreign.Request as Request
import TTHouse.Component.Lang.Data (Lang, Recipients)

import Data.Maybe (Maybe(..))
import Affjax (Error, printError)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Maybe
import Effect (Effect)
import Data.Function.Uncurried (runFn1)
import Effect.Aff (Aff)
import Data.Traversable (for)
import Effect.Class (liftEffect)
import Control.Monad.Error.Class (throwError)
import Data.Either
import Effect.Exception as Excep
import Data.Bifunctor (lmap)
import Undefined
import Data.Function.Uncurried (runFn0)
import Data.Traversable (sequence)
import Effect.AVar (AVar)
import Data.Map as Map
import Cache as Cache

data Platform = Desktop | Mobile

derive instance eqPlatform :: Eq Platform
derive instance ordPlatform :: Ord Platform

readPlatform "desktop" = Just Desktop
readPlatform "mobile" = Just Mobile
readPlatform _ = Nothing

instance showPlatform :: Show Platform where
  show Desktop = "desktop"
  show Mobile = "mobile" 

-- | We can now construct our central state which will be available to all
-- | components (if they opt-in).
-- |
-- | First, we'll use a `LogLevel` flag to indicate whether we'd like to log
-- | everything (`Dev`) or only critical messages (`Prod`). Next, we'll maintain
-- | a configurable base URL. We'll also hold on to the currently-logged-in user.
type Store = 
     { config :: Config
     , affjaxError :: Maybe Error
     , platform :: Platform
     , init :: Scaffold.ScaffoldApiControllerFrontendInitInit
     , langVar :: AVar (Map.Map Recipients Lang)
     , cache :: Cache.Cache
     }

printStore store = 
  "{config: " <> stringify (encodeJson (_.config store)) <> 
  ", affjaxError: " <> fromMaybe mempty (map printError (_.affjaxError store)) <> 
  ", platform: " <> show (_.platform store) <> 
  ", init: " <> show (_.init store) <> 
  ", langVar: <AVar (Map.Map Recipients Lang)>" <>
  ", cache: " <> show (_.cache store) <> "}"

-- | Ordinarily we'd write an initialStore function, but in our case we construct
-- | all three values in our initial store during app initialization. For that
-- | reason, you can see the store get constructed in the `Main` module.

-- | Next, we'll define a data type that represents state updates to our store.
-- | The log level and base URL should remain constant, but we'll need to be
-- | able to set the current user.
data Action = WriteAffjaxError Error | WriteMenuToCache (Map.Map String String)

-- | Finally, we'll map this action to a state update in a function called a
-- | 'reducer'. If you're curious to learn more, see the `halogen-store`
-- | documentation!
reduce :: Store -> Action -> Store
reduce store (WriteAffjaxError err) = store { affjaxError = Just err }
reduce store (WriteMenuToCache xs) = store {  cache = Cache.write xs }

initAppStore :: String -> Aff (Either Excep.Error Scaffold.ScaffoldApiControllerFrontendInitInit)
initAppStore host = Request.make host Scaffold.mkFrontApi $ runFn1 Scaffold.init
  