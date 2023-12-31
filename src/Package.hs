{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Package (parsePackage, Package, name, package) where

import Data.Map
import GHC.Generics qualified as G
import Toml
import Toml.FromValue
import Toml.FromValue.Generic

data TOMLValue
  = TOMLValueString String
  | TOMLValueInteger Integer
  | TOMLValueBoolean Bool
  | TOMLValueTable TOMLTable
  deriving (Show)

type TOMLTable = Map String TOMLValue

newtype Info = Info
  { name :: String
  }
  deriving (Show, G.Generic, Eq)

data Package = Package
  { package :: Info,
    dependencies :: Map String String
  }
  deriving (Show, G.Generic, Eq)

instance FromTable Info where fromTable = genericFromTable

instance FromValue Info where fromValue = defaultTableFromValue

instance FromTable Package where fromTable = genericFromTable

instance FromValue Package where fromValue = defaultTableFromValue

parsePackage :: String -> Result Package
parsePackage = decode
