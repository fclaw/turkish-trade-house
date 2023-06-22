module Main (main) where

import Prelude

import TTHouse.Data.Route (routeCodec)
import TTHouse.Component.Root as Root
import TTHouse.Data.Config as Cfg
import TTHouse.Api.Foreign.Scaffold (getShaCSSCommit, getShaCommit, getCookiesInit)
import TTHouse.Component.Cookie.Foreign as Cookie.Foreign

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Data.Maybe (Maybe (..), isNothing, fromMaybe)
import Effect.Aff (launchAff_, runSuspendedAff)
import Halogen (liftEffect)
import Halogen as H
import AppM as AppM
import Data.Unit
import Routing.Hash (matchesWith)
import Routing.Duplex (parse)
import Control.Monad.Error.Class (catchError, throwError)
import TTHouse.Web.Platform (getPlatform)
import Data.Function.Uncurried (runFn1)
import Web.HTML.Navigator (userAgent)
import Web.HTML.Window (navigator, document)
import Web.HTML (window)
import Store (readPlatform, initAppStore)
import Effect.Exception as Excep
import Undefined
import TTHouse.Api.Foreign.Scaffold as Scaffold
import Data.Either
import Effect.AVar as Async
import Data.Map as Map
import Effect.Console (errorShow, logShow)
import Web.DOM.Document (getElementsByTagName, createElement)
import Web.DOM.HTMLCollection (item)
import Web.DOM.Element (setAttribute)
import Web.DOM.Node (appendChild)
import Data.Traversable (for)
import Web.HTML.HTMLDocument (toDocument, toNode)
import Web.DOM.Internal.Types (Element)
import Unsafe.Coerce (unsafeCoerce)
import Cache as Cache
import Halogen.Store.Monad (getStore)
import Data.Foldable (for_)

main :: Cfg.Config -> Effect Unit
main cfg = do 
 
  ua <- window >>= navigator >>= userAgent
  platform <- map readPlatform $ runFn1 getPlatform ua
  
  when (isNothing platform) $ throwError $ Excep.error "platform type is unknown"

  HA.runHalogenAff do
      -- To run our Halogen app, we'll need two things:
      -- 1. An HTML element for Halogen to control
      -- 2. A component to mount at that HTML element, along with any arguments it requires

      -- First, we'll get a reference to the HTML element we want Halogen to attach to. Let's get a
      -- reference to the <body> tag as soon as it exists.
      body <- HA.awaitBody 

      -- request the backend to send initial values (such as static content) required to get the app running
      initResp <- initAppStore (_.scaffoldHost cfg)
      case initResp of 
        Left err -> throwError err
        Right init -> do
      
          -- I am sick to the back teeth of changing css hash manualy
          -- let's make the process a bit self-generating
          for_ (_.cssFiles cfg) $ H.liftEffect <<< setCssLink (getShaCSSCommit init) (_.cssLink cfg)

          langVar <- H.liftEffect $ Async.new Map.empty

          asyncException <- H.liftEffect $ Async.empty

          -- We now have the three pieces of information necessary to configure our app. Let's create
          -- a record that matches the `Store` type our application requires by filling in these three
          -- fields. If our environment type ever changes, we'll get a compiler error here.
          let initialStore = 
                { config: cfg { sha256Commit = getShaCommit init }
                , error: Nothing
                , platform:
                  fromMaybe 
                    undefined
                    platform 
                , init: init
                , langVar: langVar
                , cache: Cache.init
                , asyncException: asyncException
                , cookies: getCookiesInit init
                }

          -- With our app environment ready to go, we can prepare the router to run as our root component.
          --
          -- But wait! Our router is configured to run in a monad that supports all our capabilities like
          -- navigation, API requests, and logging. More concretely, we'll run the application in our
          -- custom `AppM` monad, which supports all these.
          --
          -- But Halogen only knows how to run components in the `Aff` (asynchronous effects) monad. `Aff`
          -- has no idea how to interpret our capabilities. We need a way to change our router component so
          -- that it runs in `Aff` instead of `AppM`. We can do that with `runAppM`:
          rootComponent <- AppM.runAppM initialStore Root.component

          -- Now we have the two things we need to run a Halogen application: a reference to an HTML element
          -- and the component to run there.
          --
          -- To run a Halogen application, use the `runUI` function. It accepts the component to run, arguments
          -- to provide to the component (in our case, the landing page), and the reference to an HTML element.
          -- It will start the Halogen application and return a record with two fields:
          --
          -- `query`, which lets us send queries down to the root component
          -- `subscribe`, which lets us listen and react to messages output by the root component
          --
          -- Note: Since our root component is our router, the "queries" and "messages" above refer to the
          -- `Query` and `Message` types defined in the `Conduit.Router` module. Only those queries and
          -- messages can be used, or else you'll get a compiler error.
          halogenIO <- runUI rootComponent unit body

          -- Fantastic! Our app is running and we're almost done. All that's left is to notify the router
          -- any time the location changes in the URL.
          --
          -- We're using hash-based routing, so we'll use the `matchesWith` function from `Routing.Hash` to
          -- listen for changes in the hash and parse the result (using our routing codec, `routeCodec`,
          -- along with the `parse` function from `Routing.Duplex`). Any time we parse a new location we'll
          -- trigger a `Navigate` query in the router.
          --
          -- If you feel confused by what's going on here, I'd recommend the `purescript-routing` and
          -- `purescript-routing-duplex` guides:
          --
          -- https://github.com/slamdata/purescript-routing/blob/v8.0.0/GUIDE.md
          -- https://github.com/natefaubion/purescript-routing-duplex/blob/v0.2.0/README.md
          void $ liftEffect $ matchesWith (parse routeCodec) \from to ->
             when (from /= Just to) $ launchAff_ $
               flip catchError (liftEffect <<< logShow) $
                 void $ halogenIO.query $ H.mkTell $ Root.Navigate to


-- var head  = document.getElementsByTagName('head')[0];
-- var link  = document.createElement('link');
-- link.id   = cssId;
-- link.rel  = 'stylesheet';
-- link.type = 'text/css';
-- link.href = 'http://website.example/css/stylesheet.css';
-- link.media = 'all';
-- head.appendChild(link);
setCssLink :: String -> String -> String -> Effect Unit
setCssLink sha mkHref file = do
  win <- window
  doc <- map toDocument $ document win
  xs <- "head" `getElementsByTagName` doc
  headm <- 0 `item` xs
  res <- for headm \(head :: Element) -> do 
      link :: Element <- "link" `createElement` doc
      setAttribute "rel" "stylesheet" link
      setAttribute "type" "text/css" link
      let href = mkHref <> sha <> "/" <> file <> ".css"
      setAttribute "href" href link
      appendChild (toNode (unsafeCoerce link)) (toNode (unsafeCoerce head))
      pure $ Just unit
    
  when (isNothing res) $ throwError $ Excep.error "cannot append link to head"


    

