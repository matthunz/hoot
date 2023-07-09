{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Toml (parsePackage, Package, packageName) where

import Data.Functor
import Data.Map
import Data.Map qualified as TOMLTable
import Text.Parsec
import Text.Parsec.Text

data TOMLValue
  = TOMLValueString String
  | TOMLValueInteger Integer
  | TOMLValueBoolean Bool
  | TOMLValueTable TOMLTable
  deriving (Show)

type TOMLTable = Map String TOMLValue

parseSection :: Parser String
parseSection = do
  void $ char '['
  manyTill anyChar (try (char ']'))

parseEntry :: Parser (String, String)
parseEntry = do
  key <- manyTill anyChar (try (char ' '))
  _ <- char '='
  spaces
  _ <- char '"'
  value <- manyTill anyChar (try (char '"'))
  return (key, value)

parseTable :: Parser TOMLTable
parseTable = do
  key <- parseSection
  spaces
  (entryKey, entryValue) <- parseEntry
  return (TOMLTable.fromList [(key, TOMLValueTable (TOMLTable.fromList [(entryKey, TOMLValueString entryValue)]))])

newtype Package = Package {packageName :: String} deriving (Show)

parsePackage :: Parser Package
parsePackage = do
  table <- parseTable
  let name = case table ! "package" of
        TOMLValueTable packageTable -> case packageTable ! "name" of
          TOMLValueString s -> s
          _ -> ""
        _ -> ""
  return $ Package name
