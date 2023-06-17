module System.Time (getTimestamp) where

import Prelude

import Effect


foreign import getTimestamp :: Effect Int