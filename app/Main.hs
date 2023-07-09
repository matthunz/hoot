{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer
import Iris qualified
import Lib
import Options.Applicative
import Paths_hoot as Autogen

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

newCommand :: Mod CommandFields Command
newCommand =
  command
    "new"
    (info opts (progDesc "Create a new hoot package"))
  where
    opts = NewCommand <$> strArgument (metavar "NAME" <> help "Name of the thing to create")

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

versionOption :: Parser (a -> a)
versionOption = infoOption "0.0" (long "version" <> help "Show version")

programOptions :: Parser Opts
programOptions =
  Opts
    <$> switch (long "global-flag" <> help "Set a global flag")
    <*> hsubparser (newCommand <> runCommand <> addCommand)

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
