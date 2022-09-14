module Main (main) where

import Coop.Cli.Deploy (DeployOpts (DeployOpts), deploy)
import Coop.Pab.Aux (DeployMode (DEPLOY_DEBUG))

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
    <*> strOption
      ( long "god-wallet-file"
          <> metavar "GOD_WALLET_FILE"
          <> help "God wallet"
          <> value "TODO: Sort out wallet management"
          <> showDefault
      )
    <*> strOption
      ( long "aa-wallet-file"
          <> metavar "AA_WALLET_FILE"
          <> help "AA wallet"
          <> value "TODO: Sort out wallet management"
          <> showDefault
      )
    <*> option
      auto
      ( long "aa-to-mint"
          <> metavar "AA_Q"
          <> help "$AA (authentication authority) tokens to mint"
          <> value 1
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
