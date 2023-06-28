module Store.Types
  ( LogLevel(..)
  , Platform(..)
  , readLogLevel
  , readPlatform
  )
  where

import Prelude

import Data.Maybe (Maybe (..)) 


data Platform = Desktop | Mobile

derive instance Eq Platform
derive instance Ord Platform

readPlatform "desktop" = Just Desktop
readPlatform "mobile" = Just Mobile
readPlatform _ = Nothing

instance Show Platform where
  show Desktop = "desktop"
  show Mobile = "mobile" 


data LogLevel = Dev | Prod

derive instance Eq LogLevel
derive instance Ord LogLevel

readLogLevel "dev" = Just Dev
readLogLevel "prod" = Just Prod
readLogLevel _ = Nothing

instance Show LogLevel where
  show Dev = "dev"
  show Prod = "prod"