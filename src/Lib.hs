{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( handleAdd,
    handleNew,
    handleRun,
  )
where

import Cabal (CabalPackage (..), initCabal, parseCabal, writeCabal)
import Control.Monad.Writer (runWriter)
import Package qualified
import System.Directory (createDirectory)
import System.FilePath
import System.IO
import System.Process (system)
import Text.Parsec.Text
import Toml qualified

handleNew :: FilePath -> IO ()
handleNew name = do
  createDirectory name
  writeFile (name </> "Hoot.toml") ("[package]\nname = \"" ++ name ++ "\"\n\n[dependencies]\n")

  createDirectory (name </> "src")
  writeFile (name </> "src" </> "Main.hs") "module Main (main) where\n\nmain :: IO ()\nmain = putStrLn \"Hello World!\""

handleRun :: IO ()
handleRun = do
  h <- openFile "Hoot.toml" ReadMode
  contents <- hGetContents h

  case Package.parsePackage contents of
    Toml.Failure err -> print err
    Toml.Success _ table -> do
      print table
      let packageName = Package.name $ Package.package table
      writeFile (packageName <.> "cabal") (snd $ runWriter $ initCabal packageName)

handleAdd :: [String] -> IO ()
handleAdd names = do
  let output =
        snd $
          runWriter $
            writeCabal
              CabalPackage
                { cabalVersion = "2.4",
                  name = "example",
                  version = "0.1",
                  deps = map (,">0") names
                }

  _ <- writeFile "example.cabal" output

  -- Freeze the cabal file to resolve package versions
  _ <- system "cabal freeze"

  -- Parse the dependencies from the freeze
  res <- parseFromFile parseCabal "cabal.project.freeze"
  case res of
    Left err -> putStrLn $ "Parsing error: " ++ show err
    Right result -> do
      let findMatches :: [String] -> [(String, String)] -> [(String, String)]
          findMatches strings = filter (\(x, _) -> x `elem` strings)

      -- TODO Update the package versions for the new dependencies
      mapM_ (\(name, v) -> putStrLn $ "Added " ++ name ++ " v" ++ v) (findMatches names result)
