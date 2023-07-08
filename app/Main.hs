{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Text.IO qualified as Text (putStrLn)
import Iris qualified
import Options.Applicative
import Paths_hoot as Autogen
import System.Directory
import System.FilePath

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
  | DeleteCommand

optsParser :: Parser Opts
optsParser = (helper <*> versionOption <*> programOptions)

versionOption :: Parser (a -> a)
versionOption = infoOption "0.0" (long "version" <> help "Show version")

programOptions :: Parser Opts
programOptions =
  Opts
    <$> switch (long "global-flag" <> help "Set a global flag")
    <*> hsubparser (newCommand <> deleteCommand)

newCommand :: Mod CommandFields Command
newCommand =
  command
    "new"
    (info createOptions (progDesc "Create a new hoot package"))

createOptions :: Parser Command
createOptions =
  NewCommand
    <$> strArgument (metavar "NAME" <> help "Name of the thing to create")

deleteCommand :: Mod CommandFields Command
deleteCommand =
  command
    "delete"
    (info (pure DeleteCommand) (progDesc "Delete the thing"))

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
      -- our 'Options' CLI parser
      Iris.cliEnvSettingsCmdParser = optsParser
    }

runNew :: FilePath -> IO ()
runNew name = do
  createDirectory name
  writeFile (name </> "Hoot.toml") ("[package]\nname = \"" ++ name ++ "\"\n\n[dependencies]\n")
  
  createDirectory (name </> "src")
  writeFile (name </> "src" </> "Main.hs") "module Main (main) where\n\nmain :: IO ()\nmain = putStrLn \"Hello World!\""


app :: App ()
app = do
  Opts {..} <- Iris.asksCliEnv Iris.cliEnvCmd

  let cmd = case optCommand of
        NewCommand name -> runNew name
        DeleteCommand -> putStrLn "huh"
  liftIO cmd

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app
