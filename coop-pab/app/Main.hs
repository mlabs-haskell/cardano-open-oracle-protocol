module Main (main) where

import Coop.Cli.Deploy (DeployOpts (DeployOpts), deploy)
import Coop.Pab.Aux (DeployMode (DEPLOY_DEBUG))

import Control.Applicative (Alternative (many), (<**>))
import Coop.Cli.Aux (assetClassOpt, posixTimeOpt, pubKeyHashOpt)
import Coop.Cli.GarbageCollect (GarbageCollectOpts (GarbageCollectOpts), garbageCollect)
import Coop.Cli.GetState (GetStateOpts (GetStateOpts), getState)
import Coop.Cli.MintAuth (MintAuthOpts (MintAuthOpts), mintAuth)
import Coop.Cli.MintCertRdmrs (MintCertRdmrsOpts (MintCertRdmrsOpts), mintCertRdmrs)
import Coop.Cli.TxBuilderGrpc (TxBuilderGrpcOpts (TxBuilderGrpcOpts), txBuilderService)
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
import Plutus.V1.Ledger.Value (AssetClass, adaSymbol, adaToken, assetClass)
import Plutus.V2.Ledger.Api (PubKeyHash)

data Command
  = Deploy DeployOpts
  | MintCertRdmrs MintCertRdmrsOpts
  | MintAuth MintAuthOpts
  | GarbageCollect GarbageCollectOpts
  | GetState GetStateOpts
  | TxBuilderGrpc TxBuilderGrpcOpts

pabConfigOptP :: Parser [Char]
pabConfigOptP =
  strOption
    ( long "pab-config"
        <> metavar "PAB_CONFIG"
        <> help "A bot-plutus-interface PAB config file"
        <> value "resources/pabConfig.yaml"
        <> showDefault
    )

deploymentFileOptP :: Parser [Char]
deploymentFileOptP =
  strOption
    ( long "deployment-file"
        <> metavar "DEPLOYMENT_FILE"
        <> help "A JSON file to write the COOP deployment information to"
        <> value "resources/coop-deployment.json"
        <> showDefault
    )

modeOptP :: Parser DeployMode
modeOptP =
  option
    auto
    ( long "mode"
        <> metavar "DEPLOY_MODE"
        <> help "Compilation mode for Plutus scripts that enables tracing logs (ie. DEPLOY_DEBUG|DEPLOY_PROD)"
        <> value DEPLOY_DEBUG
        <> showDefault
    )

aaWalletPkhOptP :: Parser PubKeyHash
aaWalletPkhOptP =
  pubKeyHashOpt
    ( long "aa-wallet"
        <> metavar "AA_WALLET"
        <> help "A wallet hexed PubKeyHash (eq. 04efa495982b94e07511eaa07c738a0a7ec356729e4b751159d96001) holding $AA tokens"
    )

certRdmrAcOptP :: Parser AssetClass
certRdmrAcOptP =
  assetClassOpt
    ( long "cert-rdmr-ac"
        <> metavar "CERTRDMR_AC"
        <> help "$CERT-RDMR asset class that can be used to garbage collect expired $CERT UTxOs locked at @CertV"
    )

certRdmrWalletOptP :: Parser PubKeyHash
certRdmrWalletOptP =
  pubKeyHashOpt
    ( long "cert-rdmr-wallet"
        <> metavar "CERTRDMR_WALLET"
        <> help "A wallet hexed PubKeyHash (eq. 04efa495982b94e07511eaa07c738a0a7ec356729e4b751159d96001) holding $CERT-RDMR tokens that will perform `coop-pab-cli garbage-collect`"
    )

authWalletsOpt :: Parser [PubKeyHash]
authWalletsOpt =
  many
    ( pubKeyHashOpt
        ( long "auth-wallet"
            <> metavar "AUTH_WALLET"
            <> help "Wallet hexed PubKeyHash (eq. 04efa495982b94e07511eaa07c738a0a7ec356729e4b751159d96001) holding $AUTH tokens"
        )
    )

feeOptP :: Parser (PubKeyHash, AssetClass, Integer)
feeOptP =
  (,,)
    <$> pubKeyHashOpt
      ( long "fee-wallet"
          <> metavar "FEE_WALLET"
          <> help "Wallet hexed PubKeyHash (eq. 04efa495982b94e07511eaa07c738a0a7ec356729e4b751159d96001) holding $FEE tokens"
      )
      <*> assetClassOpt
        ( long "fee-ac"
            <> metavar "FEE_AC"
            <> help "$FEE asset class used to pay the COOP Publisher for publishing Fact Statements"
            <> value (assetClass adaSymbol adaToken)
            <> showDefault
        )
      <*> option
        auto
        ( long "fee-quantity"
            <> metavar "FEE_Q"
            <> help "$FEE amount to pay the COOP Publisher for publishing Fact Statements"
            <> value 1
            <> showDefault
        )

deployOptsP :: Parser DeployOpts
deployOptsP =
  DeployOpts
    <$> modeOptP
    <*> pabConfigOptP
    <*> deploymentFileOptP
    <*> pubKeyHashOpt
      ( long "god-wallet"
          <> metavar "GOD_WALLET"
          <> help "A wallet hexed PubKeyHash (eq. 04efa495982b94e07511eaa07c738a0a7ec356729e4b751159d96001) used to perform the COOP genesis"
      )
    <*> aaWalletPkhOptP
    <*> option
      auto
      ( long "at-least-aa-required"
          <> metavar "AA_Q_REQUIRED"
          <> help "$AA (authentication authority) token quantity required to mint authentication (ie. `coop-pab-cli mint-auth`)"
          <> value 1
          <> showDefault
      )
    <*> option
      auto
      ( long "aa-to-mint"
          <> metavar "AA_Q"
          <> help "$AA (authentication authority) tokens to mint and pay to AA_WALLET"
          <> value 3
          <> showDefault
      )

mintCertRdmrsOptsP :: Parser MintCertRdmrsOpts
mintCertRdmrsOptsP =
  MintCertRdmrsOpts
    <$> modeOptP
    <*> pabConfigOptP
    <*> certRdmrWalletOptP
    <*> option
      auto
      ( long "cert-rdmrs-to-mint"
          <> metavar "CERTRDMR_Q"
          <> help "$CERT-RDMR (certificate redeemer) tokens to mint and pay to CERTRDMR_WALLET"
          <> value 100
          <> showDefault
      )

mintAuthOptsP :: Parser MintAuthOpts
mintAuthOptsP =
  MintAuthOpts
    <$> pabConfigOptP
    <*> deploymentFileOptP
    <*> aaWalletPkhOptP
    <*> posixTimeOpt
      ( long "certificate-valid-from"
          <> metavar "CERT_VALID_FROM"
          <> help "POSIXTime denoting the Ledger time the certificate is valid from"
      )
    <*> posixTimeOpt
      ( long "certificate-valid-to"
          <> metavar "CERT_VALID_TO"
          <> help "POSIXTime denoting the Ledger time the certificate is valid until"
      )
    <*> option
      auto
      ( long "auth-tokens-per-wallet-to-mint"
          <> metavar "AUTH_Q_PER_WALLET"
          <> help "$AUTH tokens to mint and pay to each specified Auth wallet (AUTH_WALLET)"
          <> value 100
          <> showDefault
      )
    <*> certRdmrAcOptP
    <*> authWalletsOpt

garbageCollectOptsP :: Parser GarbageCollectOpts
garbageCollectOptsP =
  GarbageCollectOpts
    <$> pabConfigOptP
    <*> deploymentFileOptP
    <*> certRdmrWalletOptP
    <*> certRdmrAcOptP

getStateOptsP :: Parser GetStateOpts
getStateOptsP =
  GetStateOpts
    <$> pabConfigOptP
    <*> deploymentFileOptP
    <*> strOption
      ( long "state-file"
          <> metavar "STATE_FILE"
          <> help "A JSON file to write the COOP state information to"
          <> value "resources/coop-state.json"
          <> showDefault
      )

txBuilderGrpcOpts :: Parser TxBuilderGrpcOpts
txBuilderGrpcOpts =
  TxBuilderGrpcOpts
    <$> pabConfigOptP
    <*> deploymentFileOptP
    <*> authWalletsOpt
    <*> feeOptP
    <*> strOption
      ( long "address"
          <> metavar "ADDR"
          <> help "Local IP address or host name to bing the TxBuilder gRpc service to"
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "port"
          <> metavar "PORT"
          <> help "TCP port to bind the TxBuilder gRpc service to"
          <> value 5081
          <> showDefault
      )
    <*> strOption
      ( long "cert-file"
          <> metavar "CERT_FILE"
          <> help "Certificate file to use for TLS"
          <> value "resources/certificate.pem"
          <> showDefault
      )
    <*> strOption
      ( long "key-file"
          <> metavar "KEY_FILE"
          <> help "Private key file to use for TLS"
          <> value "resources/key.pem"
          <> showDefault
      )

optionsP :: Parser Command
optionsP =
  subparser $
    command
      "deploy"
      (info (Deploy <$> deployOptsP <* helper) (progDesc "Deploy COOP on the Cardano network and write the deployment information to a file"))
      <> command
        "mint-cert-redeemers"
        (info (MintCertRdmrs <$> mintCertRdmrsOptsP <* helper) (progDesc "Mint $CERT-RDMR (one shot) tokens and pay them to a wallet that will perform garbage collection"))
      <> command
        "mint-auth"
        (info (MintAuth <$> mintAuthOptsP <* helper) (progDesc "Mint and pay to @CertV a $CERT token with a specified $CERT-RDMR asset class and validty range along with minting and paying associated $AUTH tokens to each Auth wallet"))
      <> command
        "garbage-collect"
        (info (GarbageCollect <$> garbageCollectOptsP <* helper) (progDesc "Spend expired $CERT tokens locked at @CertV using $CERT-RDMR tokens"))
      <> command
        "get-state"
        (info (GetState <$> getStateOptsP <* helper) (progDesc "Get COOP state"))
      <> command
        "tx-builder-grpc"
        (info (TxBuilderGrpc <$> txBuilderGrpcOpts <* helper) (progDesc "Run a TxBuilder gRpc service"))

parserInfo :: ParserInfo Command
parserInfo = info (optionsP <**> helper) (fullDesc <> progDesc "COOP PAB cli tools")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    Deploy opts -> deploy opts
    MintCertRdmrs opts -> mintCertRdmrs opts
    MintAuth opts -> mintAuth opts
    GarbageCollect opts -> garbageCollect opts
    GetState opts -> getState opts
    TxBuilderGrpc opts -> txBuilderService opts
