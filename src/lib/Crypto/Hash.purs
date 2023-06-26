module Crypto.Hash (createHash) where

import Prelude

import System.Time (getTimestamp)
import Effect (Effect)

foreign import _createHash :: String -> Effect String

createHash :: Effect String
createHash = do
  tm <- getTimestamp 
  _createHash $ show tm