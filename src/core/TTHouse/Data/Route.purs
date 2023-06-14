-- | We can represent valid locations in our application with a simple sum type. This will cause
-- | any invalid routes to fail at compile-time.
-- |
-- | But since the browser represents locations with strings, we'll also need a way to write our
-- | `Route` type to a string and parse strings into valid `Route` values. It's tedious and error-
-- | prone to maintain separate printing and parsing functions which can fall out of sync with
-- | another, and even worse to write them manually. Fortunately, the `routing-duplex` library will
-- | help us write a bi-directional codec which solves both problems.
-- |
-- | For more information about the library and to read the tutorial, see:
-- | https://github.com/natefaubion/purescript-routing-duplex/tree/v0.2.0
module TTHouse.Data.Route
  ( Route(..)
  , routeCodec
  ) where

import Prelude hiding ((/))

import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', as, root, segment, param)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Data.Show
import Data.Enum
import Data.Maybe
import Data.Bounded
import Data.Enum.Generic (genericFromEnum, genericToEnum, genericSucc, genericPred, genericCardinality)

-- | We'll represent routes in our application with a simple sum type. As the application grows,
-- | you might want to swap this out with an extensible sum type with `Variant` and have several
-- | sub-sections. For our small MVP this type will work just fine and will prevent us from trying
-- | to send users to non-existent routes.
data Route
  = Error
  | Home
  | About
  | Service 
  | Feedback

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show Home = "home"
  show Error = "error"
  show About = "about"
  show Service = "service"
  show Feedback = "feedback"

instance enumRoute :: Enum Route where 
  succ = genericSucc
  pred = genericPred 

instance boundedEnumRoute :: BoundedEnum Route where 
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance boundedRoute :: Bounded Route where 
  top = Feedback
  bottom = Error

-- | Next, we'll define a bidirectional codec for our route parsing. Our single codec will handle
-- | both parsing browser locations and serializing our data type to a browser location. We'll skip
-- | the boilerplate of separate encoding and decoding functions, and we'll ensure our parsing and
-- | printing is always in sync.
-- |
-- | Our codec will cause a compile-time error if we fail to handle any of our route cases.
routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "Error": "error" / noArgs
  , "About": "about" / noArgs
  , "Service": "service" / noArgs
  , "Feedback": "feedback" / noArgs
  }