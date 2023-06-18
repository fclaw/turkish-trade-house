module TTHouse.Page.Error
  ( component
  , proxy
  )
  where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Halogen.Store.Monad (getStore)
import Effect.Exception (message)
import Data.Foldable (for_)

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
         for_ error \e -> H.modify_ _ { msg = message e }

render { msg } = HH.div_ [ HH.text msg ]