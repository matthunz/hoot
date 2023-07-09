module Cabal (initCabal, CabalPackage(..), writeCabal, parseCabal) where

import Control.Monad.Writer
import Text.Parsec.Text
import Text.Parsec


parseDep :: Parser (String, String)
parseDep = do
  _ <- manyTill anyChar (try (string " any."))
  n <- manyTill anyChar (try (string " =="))
  v <- manyTill anyChar (try (string "," <|> string "\n"))
  return (n, v)

parseCabal :: Parser [(String, String)]
parseCabal = many . try $ parseDep


field :: String -> String -> Writer String ()
field key value = do
  tell key
  tell ": "
  tell value
  tell "\n"

initCabal :: String -> Writer String ()
initCabal name = do
  field "cabal-version" "2.4"
  field "name" name
  field "version" "0.1.0.0"
  tell "executable "
  tell name
  tell "\n"
  tell "  "
  field "main-is" "Main.hs"
  tell "  "
  field "hs-source-dirs" "src"
  tell "  "
  field "build-depends" "base ^>=4.16.4.0"
  tell "  "
  field "default-languag" "Haskell2010"

data CabalPackage = CabalPackage
  { cabalVersion :: String,
    name :: String,
    version :: String,
    deps :: [(String, String)]
  }

writeCabal :: CabalPackage -> Writer String ()
writeCabal cabal = do
  field "cabal-version" "2.4"
  field "name" (name cabal)
  field "version" "0.1.0.0"
  tell "executable "
  tell (name cabal)
  tell "\n"
  tell "  "
  field "main-is" "Main.hs"
  tell "  "
  field "hs-source-dirs" "src"
  tell "  "
  field "build-depends" $ concatMap (\(x, y) -> x ++ " " ++ y ++ ",") (deps cabal)
  tell "  "
  field "default-languag" "Haskell2010"
