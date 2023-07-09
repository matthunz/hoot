{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( parseCabal,
  )
where

import Text.Parsec
import Text.Parsec.Text

parseDep :: Parser (String, String)
parseDep = do
  _ <- manyTill anyChar (try (string " any."))
  name <- manyTill anyChar (try (string " =="))
  version <- manyTill anyChar (try (string ","))
  return (name, version)

parseCabal :: Parser [(String, String)]
parseCabal = many . try $ parseDep
