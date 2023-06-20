module TTHouse.Component.AsyncException (component, proxy, AsyncErrorMag (..))  where

import Prelude

import TTHouse.Component.HTML.Utils (css)
import TTHouse.Capability.LogMessages (logError)

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Halogen.HTML.Properties.Extended as HP
import DOM.HTML.Indexed.ButtonType (ButtonType (ButtonButton))
import Halogen.HTML.Events (onClick)
import Effect.Aff as Aff
import Data.Foldable (for_)
import Effect.AVar as Async
import Effect.Exception (Error, message)
import Halogen.Store.Monad (getStore)
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe (..))
import Data.Map as Map
import Data.Functor ((<#>))
import Data.Tuple (Tuple (..))

proxy = Proxy :: _ "async-exception"

data Action = Close Int | Add AsyncErrorMag | Initialize

type State = { xs :: Map.Map Int AsyncErrorMag }

type AsyncErrorMag = { err :: Error, loc :: String }


component =
  H.mkComponent
    { initialState: const { xs: Map.empty }
    , render: render
    , eval: H.mkEval H.defaultEval 
      { handleAction = handleAction
      , initialize = pure Initialize }
    }
  where
    handleAction Initialize = do
      { asyncException } <- getStore
      void $ H.fork $ forever $ do
        H.liftAff $ Aff.delay $ Aff.Milliseconds 1000.0
        val <- H.liftEffect $ Async.tryTake asyncException
        for_ val $ handleAction <<< Add 
    handleAction (Add e) = 
      H.modify_ \s -> do 
        let m = Map.findMax (_.xs s)
        let newXs = 
             case m of 
               Just { key } -> Map.insert (key + 1) e (_.xs s)
               Nothing -> Map.singleton 1 e
        s { xs = newXs }
    handleAction (Close idx) = H.modify_ \s -> s { xs = Map.delete idx (_.xs s) }


render { xs } = 
  HH.div_ $ 
    (Map.toUnfoldable xs) <#> \(Tuple k { err, loc }) ->
      let margin = show $ (k - 1) * 50
      in HH.div
         [ onClick $ const (Close k)
         , css "alert alert-danger alert-position"
         , HP.style ("margin-top:" <> margin <> "px")
         , HP.role "alert"] 
         [ HH.text (message err <> " at " <> loc) ]