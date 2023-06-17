module Statistics (sendComponentTime) where

import Prelude

import  TTHouse.Capability.LogMessages (logDebug)

import Effect

sendComponentTime start end component = logDebug $ "(" <> component <> ") component has been closed at " <> show end <> ", total time: " <> show (end - start) <> "s"