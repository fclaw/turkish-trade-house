module TTHouse.Component.Menu.Hamburger ( component, proxy ) where

import Prelude

import TTHouse.Capability.LogMessages (logDebug)
import TTHouse.Component.HTML.Utils (css, safeHref)
import TTHouse.Data.Route (Route (..))

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import DOM.HTML.Indexed.InputType
import Halogen.HTML.Properties.Extended as HPExt
import Routing.Duplex (print)
import Data.Array ((..))
import Data.Enum (fromEnum, toEnum)
import Data.Maybe
import Undefined
import Halogen.Subscription as HS
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)
import Web.HTML.Window (innerWidth)
import Web.HTML (window)
import Control.Monad.Rec.Class (forever)
import Effect.Aff (Milliseconds(..))

import Effect.Console (logShow)

proxy = Proxy :: _ "hamburgerMenu"

type State = { outerWinwidth :: Maybe Int }

data Action =  Initialise | GetWindowWidth Int

component =
  H.mkComponent
    { initialState: const { outerWinwidth: Nothing }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialise 
      }
    }

handleAction Initialise = void $ H.subscribe =<< subscribe GetWindowWidth
  where 
    subscribe go  = do 
      { emitter, listener } <- H.liftEffect HS.create
      void $ H.fork $ forever $ do 
        liftAff $ Aff.delay $ Milliseconds 500.0
        w <- H.liftEffect $ window >>= innerWidth
        logDebug $ "window width: " <> show w
        H.liftEffect $ HS.notify listener $ go w
      pure emitter
handleAction (GetWindowWidth w) = H.modify_ \s -> s { outerWinwidth = pure w }

-- I piggyback on the following implementation https://codepen.io/alvarotrigo/pen/PoJGObg
render { outerWinwidth } 
  | fromMaybe true $ map ((>) 500) (outerWinwidth) =
      HH.div_
      [
         HH.input [HPExt.type_ InputCheckbox, css "toggler"]
      ,  HH.div [css "hamburger"] [HH.div_ []]
      ,  HH.div [css "menu"]
         [
            HH.div_
            [
                  HH.ul_ (map item (fromEnum Home .. fromEnum Service) )
            ]
         ]     
      ]
  | otherwise = HH.div_ []   

item idx = HH.li_ [HH.a [safeHref (mkRoute idx) ] [HH.text (show (mkRoute idx))] ] 
  where mkRoute = fromMaybe undefined <<< (toEnum :: Int -> Maybe Route)