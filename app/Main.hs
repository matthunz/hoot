{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, ImportQualifiedPost #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Text.IO qualified as Text (putStrLn)
import Iris qualified
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
    { -- short description
      Iris.cliEnvSettingsHeaderDesc = "Iris usage example",
      -- longer description
      Iris.cliEnvSettingsProgDesc = "A simple grep utility - tutorial example",
      -- a function to display the tool version
      Iris.cliEnvSettingsVersionSettings =
        Just
          (Iris.defaultVersionSettings Autogen.version)
            { Iris.versionSettingsMkDesc = \v -> "Simple grep utility v" <> v
            },
      -- our 'Options' CLI parser
      Iris.cliEnvSettingsCmdParser = optsParser
    }

app :: App ()
app = do
  -- 1. Get parsed 'Options' from the environment
  Opts {..} <- Iris.asksCliEnv Iris.cliEnvCmd
  liftIO $ Text.putStrLn "WAT"

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app
