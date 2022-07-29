{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Main (main) where

import Cardano.Oracle.Cli.Compile (CompileMode (CMDEBUG, COMPILE_DEBUG), CompileOpts (CompileOpts), compile)

import Control.Applicative ((<**>))
import Options.Applicative (
  Applicative ((<*>)),
  Parser,
  ParserInfo,
  auto,
  command,
  customExecParser,
  flag',
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  prefs,
  progDesc,
  showDefault,
  showHelpOnEmpty,
  showHelpOnError,
  strOption,
  subparser,
  value,
  (<$>),
 )

data Command
  = Compile CompileOpts

compileOpts :: Parser CompileOpts
compileOpts =
  CompileOpts
    <$> option
      auto
      ( long "mode"
          <> metavar "COMPILE_MODE"
          <> help "Mode of compilation COMPILE_DEBUG|COMPILE_DEBUG"
          <> value COMPILE_DEBUG
          <> showDefault
      )
    <*> strOption
      ( long "file"
          <> metavar "COMPILE_FILE"
          <> help "A JSON file to store the compiled scripts"
          <> value "coop-plutus.json"
          <> showDefault
      )

options :: Parser Command
options =
  subparser $
    command
      "compile"
      (info (Compile <$> compileOpts <* helper) (progDesc "Compile scripts and write them to a file"))

parserInfo :: ParserInfo Command
parserInfo = info (options <**> helper) (fullDesc <> progDesc "COOP Plutus cli tools")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    Compile opts -> compile opts
