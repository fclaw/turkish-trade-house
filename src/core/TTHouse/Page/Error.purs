module TTHouse.Page.Error
  ( component
  , proxy
  )
  where

import Prelude

import TTHouse.Component.HTML.Utils (css)

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Halogen.Store.Monad (getStore)
import Effect.Exception (message)
import Data.Foldable (for_)
import TTHouse.Capability.Navigate (navigate)
import TTHouse.Data.Route (Route(Home))
import Data.Maybe (isNothing)

proxy = Proxy :: _ "error"

type State = { msg :: String }

data Action = Initialize

component =
  H.mkComponent
    { initialState: const { msg: mempty }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize 
      }
    }
    where 
       handleAction Initialize = do 
         {error} <- getStore
         when (isNothing error) $ navigate Home
         for_ error \e -> H.modify_ _ { msg = message e }

render { msg } = 
  HH.section [css "centered"] 
  [ 
      HH.h1_ [ HH.text "Server Error" ]
  ,   HH.div [css "container"] [HH.div_ [HH.text msg]]
  ]