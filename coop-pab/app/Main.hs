module Main (main) where

import Coop.Cli.Deploy (DeployOpts (DeployOpts), deploy)
import Coop.Pab.Aux (DeployMode (DEPLOY_DEBUG))

import Control.Applicative (Alternative (many), (<**>))
import Coop.Cli.Aux (assetClassOpt, posixTimeOpt, pubKeyHashOpt)
import Coop.Cli.GarbageCollect (GarbageCollectOpts (GarbageCollectOpts), garbageCollect)
import Coop.Cli.MintAuth (MintAuthOpts (MintAuthOpts), mintAuth)
import Coop.Cli.MintCertRdmrs (MintCertRdmrsOpts (MintCertRdmrsOpts), mintCertRdmrs)
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

data Command
  = Deploy DeployOpts
  | MintCertRdmrs MintCertRdmrsOpts
  | MintAuth MintAuthOpts
  | GarbageCollect GarbageCollectOpts

deployOpts :: Parser DeployOpts
deployOpts =
  DeployOpts
    <$> option
      auto
      ( long "mode"
          <> metavar "DEPLOY_MODE"
          <> help "Compilation mode for Plutus scripts (ie. DEPLOY_DEBUG|DEPLOY_PROD)"
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
    <*> pubKeyHashOpt
      ( long "god-wallet"
          <> metavar "GOD_WALLET"
          <> help "God wallet hexed PubKeyHash (eq. 04efa495982b94e07511eaa07c738a0a7ec356729e4b751159d96001)"
      )
    <*> pubKeyHashOpt
      ( long "aa-wallet"
          <> metavar "AA_WALLET"
          <> help "AA wallet hexed PubKeyHash (eq. 04efa495982b94e07511eaa07c738a0a7ec356729e4b751159d96001)"
      )
    <*> option
      auto
      ( long "aa-to-mint"
          <> metavar "AA_Q"
          <> help "$AA (authentication authority) tokens to mint"
          <> value 1
          <> showDefault
      )

mintCertRdmrsOpts :: Parser MintCertRdmrsOpts
mintCertRdmrsOpts =
  MintCertRdmrsOpts
    <$> option
      auto
      ( long "mode"
          <> metavar "DEPLOY_MODE"
          <> help "Compilation mode for Plutus scripts (ie. DEPLOY_DEBUG|DEPLOY_PROD)"
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
    <*> pubKeyHashOpt
      ( long "cert-rdmr-wallet"
          <> metavar "CERTRDMR_WALLET"
          <> help "A wallet hexed PubKeyHash (eq. 04efa495982b94e07511eaa07c738a0a7ec356729e4b751159d96001) holding $CERT-RDMR tokens"
      )
    <*> option
      auto
      ( long "cert-rdmrs-to-mint"
          <> metavar "CERTRDMR_Q"
          <> help "$CERT-RDMR (certificate redeemer) tokens to mint"
          <> value 100
          <> showDefault
      )

mintAuthOpts :: Parser MintAuthOpts
mintAuthOpts =
  MintAuthOpts
    <$> strOption
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
    <*> pubKeyHashOpt
      ( long "aa-wallet"
          <> metavar "AA_WALLET"
          <> help "AA wallet hexed PubKeyHash (eq. 04efa495982b94e07511eaa07c738a0a7ec356729e4b751159d96001)"
      )
    <*> posixTimeOpt
      ( long "certificate-valid-from"
          <> metavar "CERT_VALID_FROM"
          <> help "Certificate valid from POSIXTime"
      )
    <*> posixTimeOpt
      ( long "certificate-valid-to"
          <> metavar "CERT_VALID_TO"
          <> help "Certificate valid to POSIXTime"
      )
    <*> option
      auto
      ( long "n-auth-tokens-per-wallet"
          <> metavar "AUTH_Q_PER_WALLET"
          <> help "$AUTH tokens to mint per wallet"
          <> value 100
          <> showDefault
      )
    <*> assetClassOpt
      ( long "cert-rdmr-ac"
          <> metavar "CERTRDMR_AC"
          <> help "$CERT-RDMR asset class that can be used to garbage collect expired $CERT UTxOs locked at @CertV"
      )
    <*> many
      ( pubKeyHashOpt
          ( long "auth-wallet"
              <> metavar "AUTH_WALLET"
              <> help "Wallet holding $AUTH tokens hexed PubKeyHash (eq. 04efa495982b94e07511eaa07c738a0a7ec356729e4b751159d96001)"
          )
      )

garbageCollectOpts :: Parser GarbageCollectOpts
garbageCollectOpts =
  GarbageCollectOpts
    <$> strOption
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
    <*> pubKeyHashOpt
      ( long "cert-rdmr-wallet"
          <> metavar "CERTRDMR_WALLET"
          <> help "A wallet hexed PubKeyHash (eq. 04efa495982b94e07511eaa07c738a0a7ec356729e4b751159d96001) holding $CERT-RDMR tokens"
      )
    <*> assetClassOpt
      ( long "cert-rdmr-ac"
          <> metavar "CERTRDMR_AC"
          <> help "$CERT-RDMR asset class that can be used to garbage collect expired $CERT UTxOs locked at @CertV"
      )

options :: Parser Command
options =
  subparser $
    command
      "deploy"
      (info (Deploy <$> deployOpts <* helper) (progDesc "Deploy COOP on the Cardano network and write the deployment information to a file"))
      <> command
        "mint-cert-redeemers"
        (info (MintCertRdmrs <$> mintCertRdmrsOpts <* helper) (progDesc "Mint CERTRDMR_Q of $CERT-RDMR (one shot) tokens and pay them to the CERTRDMR_WALLET"))
      <> command
        "mint-auth"
        (info (MintAuth <$> mintAuthOpts <* helper) (progDesc "Mint and pay AUTH_Q_PER_WALLET $CERT and $AUTH tokens to each AUTH_WALLET and assign the cert validity and $CERT-RDMR asset class"))
      <> command
        "garbage-collect"
        (info (GarbageCollect <$> garbageCollectOpts <* helper) (progDesc "Burn expired $CERT tokens locked at @CertV using $CERT-RDMR tokens"))

parserInfo :: ParserInfo Command
parserInfo = info (options <**> helper) (fullDesc <> progDesc "COOP PAB cli tools")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    Deploy opts -> deploy opts
    MintCertRdmrs opts -> mintCertRdmrs opts
    MintAuth opts -> mintAuth opts
    GarbageCollect opts -> garbageCollect opts
