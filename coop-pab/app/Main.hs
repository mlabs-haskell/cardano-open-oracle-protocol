module Main (main) where

import Cardano.Oracle.Cli.Deploy (DeployOpts (DeployOpts), deploy)
import Cardano.Oracle.Pab.Aux (DeployMode (DEPLOY_DEBUG))

import Control.Applicative ((<**>))
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  command,
  customExecParser,
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
 )

newtype Command
  = Deploy DeployOpts

deployOpts :: Parser DeployOpts
deployOpts =
  DeployOpts
    <$> option
      auto
      ( long "mode"
          <> metavar "DEPLOY_MODE"
          <> help "Mode of deployment DEPLOY_DEBUG|DEPLOY_PROD"
          <> value DEPLOY_DEBUG
          <> showDefault
      )
    <*> strOption
      ( long "pab-config"
          <> metavar "PAB_CONFIG"
          <> help "A bot-plutus-interface PAB config file"
          <> value "resources/pabConfig.yaml"
          <> showDefault
      )
    <*> strOption
      ( long "deployment-file"
          <> metavar "DEPLOYMENT_FILE"
          <> help "A JSON file to write the deployment information to"
          <> value "coop-deployment.json"
          <> showDefault
      )

options :: Parser Command
options =
  subparser $
    command
      "deploy"
      (info (Deploy <$> deployOpts <* helper) (progDesc "Deploy COOP on the Cardano network and write the deployment information a file"))

parserInfo :: ParserInfo Command
parserInfo = info (options <**> helper) (fullDesc <> progDesc "COOP PAB cli tools")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    Deploy opts -> deploy opts
