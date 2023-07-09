{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Cabal
import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer
import Iris qualified
import Lib (parseCabal)
import Options.Applicative
import Paths_hoot as Autogen
import System.Directory (createDirectory)
import System.FilePath
import System.Process (system)
import Text.Parsec.Text (parseFromFile)
import qualified Package
import System.IO
import qualified Toml

newtype App a = App
  { unApp :: Iris.CliApp Opts () a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (Iris.CliEnv Opts ())
    )

data Opts = Opts
  { optGlobalFlag :: !Bool,
    optCommand :: !Command
  }

data Command
  = NewCommand String
  | RunCommand
  | AddCommand [String]

versionOption :: Parser (a -> a)
versionOption = infoOption "0.0" (long "version" <> help "Show version")

programOptions :: Parser Opts
programOptions =
  Opts
    <$> switch (long "global-flag" <> help "Set a global flag")
    <*> hsubparser (newCommand <> runCommand <> addCommand)

newCommand :: Mod CommandFields Command
newCommand =
  command
    "new"
    (info createOptions (progDesc "Create a new hoot package"))

createOptions :: Parser Command
createOptions =
  NewCommand
    <$> strArgument (metavar "NAME" <> help "Name of the thing to create")

runCommand :: Mod CommandFields Command
runCommand =
  command
    "run"
    (info (pure RunCommand) (progDesc "Run a binary or example of the local package"))

addCommand :: Mod CommandFields Command
addCommand =
  command
    "add"
    (info opts (progDesc "Create a new hoot package"))
  where
    opts :: Parser Command
    opts = AddCommand <$> some (argument str (metavar "NAME" <> help "Name of the packages to add"))

optsParser :: Parser Opts
optsParser = helper <*> versionOption <*> programOptions

appSettings :: Iris.CliEnvSettings Opts ()
appSettings =
  Iris.defaultCliEnvSettings
    { Iris.cliEnvSettingsHeaderDesc = "Hoot haskell package manager",
      Iris.cliEnvSettingsProgDesc = "An opionated haskell package manager based on cabal.",
      Iris.cliEnvSettingsVersionSettings =
        Just
          (Iris.defaultVersionSettings Autogen.version)
            { Iris.versionSettingsMkDesc = ("Hoot v" <>)
            },
      Iris.cliEnvSettingsCmdParser = optsParser
    }

handleNew :: FilePath -> IO ()
handleNew name = do
  createDirectory name
  writeFile (name </> "Hoot.toml") ("[package]\nname = \"" ++ name ++ "\"\n\n[dependencies]\n")

  createDirectory (name </> "src")
  writeFile (name </> "src" </> "Main.hs") "module Main (main) where\n\nmain :: IO ()\nmain = putStrLn \"Hello World!\""

handleRun :: IO ()
handleRun = do
  h <- openFile "Hoot.toml" ReadMode
  contents <-  hGetContents h

  case Package.parsePackage contents of
    Toml.Failure err -> print err
    Toml.Success _ table -> do
      print table
      let packageName = Package.name $ Package.package table
      writeFile ( packageName <.> "cabal") (snd $ runWriter $ Cabal.initCabal $ packageName)


handleAdd :: [String] -> IO ()
handleAdd names = do
  -- Freeze the cabal file to resolve package versions
  _ <- system "cabal freeze"

  -- TODO Add the new packages to the cabal file

  -- Parse the dependencies from the freeze
  res <- parseFromFile parseCabal "cabal.project.freeze"
  case res of
    Left err -> putStrLn $ "Parsing error: " ++ show err
    Right result -> do
      let findMatches :: [String] -> [(String, String)] -> [(String, String)]
          findMatches strings = filter (\(x, _) -> x `elem` strings)

      -- TODO Update the package versions for the new dependencies
      mapM_ (\(name, v) -> putStrLn $ "Added " ++ name ++ " v" ++ v) (findMatches names result)

app :: App ()
app = do
  Opts {..} <- Iris.asksCliEnv Iris.cliEnvCmd
  let cmd = case optCommand of
        NewCommand name -> handleNew name
        RunCommand -> handleRun
        AddCommand names -> handleAdd names
  liftIO cmd

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app
