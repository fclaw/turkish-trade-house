module TTHouse.Component.Async
  ( Async
  , Value
  , Level (..)
  , component
  , mkException
  , mkOrdinary
  , proxy
  , send
  )
  where

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
import Data.Maybe (Maybe (..), maybe)
import Data.Map as Map
import Data.Functor ((<#>))
import Data.Tuple (Tuple (..))
import Data.List (zip, fromFoldable, length)
import Data.Array ((..))

proxy = Proxy :: _ "async"

data Action = Close Int | Add Async | Initialize

type State = { xs :: Map.Map Int Async }

data Level = Warning | Success | Info

data Value = Exception Error | Ordinary String Level 

type Async = { val :: Value, loc :: Maybe String }

mkException error loc = { val: Exception error, loc: Just loc }
mkOrdinary msg level loc = { val: Ordinary msg level, loc: loc }

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
      { async } <- getStore
      void $ H.fork $ forever $ do
        H.liftAff $ Aff.delay $ Aff.Milliseconds 1000.0
        val <- H.liftEffect $ Async.tryTake async
        for_ val $ handleAction <<< Add 
    handleAction (Add e) = 
      H.modify_ \s -> do 
        let m = Map.findMax (_.xs s)
        let newXs = 
             case m of 
               Just { key } -> Map.insert (key + 1) e (_.xs s)
               Nothing -> Map.singleton 1 e
        s { xs = newXs }
    handleAction (Close idx) = H.modify_ \s -> s { xs = recalculateIdx $ Map.delete idx (_.xs s) }

render { xs } = 
  HH.div_ $ 
    (Map.toUnfoldable xs) <#> \(Tuple k { val, loc }) ->
      let margin = show $ (k - 1) * 50
      in HH.div
         [ onClick $ const (Close k)
         , css $ "alert " <> mkStyle val <> " alert-position"
         , HP.style ("margin-top:" <> margin <> "px")
         , HP.role "alert"] 
         [ HH.text (mkMsg val <> maybe mempty (\s -> " at " <> s) loc) ]
  where
    mkStyle val = 
      case val of 
        Exception _ -> "alert-danger"
        Ordinary _ Warning -> "alert-warning"
        Ordinary _ Success -> "alert-success"
        Ordinary _ Info -> "alert-info"
    mkMsg val = 
      case val of 
        Exception err -> message err
        Ordinary msg _ -> msg

recalculateIdx xs = 
  let valXs = Map.values xs
      idXs = fromFoldable (1 .. length valXs)
  in Map.fromFoldable $ zip idXs valXs

send val = do 
  { async } <- getStore
  void $ H.liftEffect $ Async.tryPut val async