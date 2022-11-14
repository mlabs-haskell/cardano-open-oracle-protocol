module Main (main) where

import Control.Applicative ((<**>))
import Coop.Cli.PublisherGrpc (PublisherGrpcOpts (PublisherGrpcOpts), publisherService)
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
  = PublisherGrpc PublisherGrpcOpts

publisherGrpcOpts :: Parser PublisherGrpcOpts
publisherGrpcOpts =
  PublisherGrpcOpts
    <$> strOption
      ( long "address"
          <> metavar "ADDR"
          <> help "Local IP address or host name to bing the Publisher gRpc service to"
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "port"
          <> metavar "PORT"
          <> help "TCP port to bind the Publisher gRpc service to"
          <> value 5080
          <> showDefault
      )
    <*> strOption
      ( long "cert-file"
          <> metavar "CERT_FILE"
          <> help "Certificate file to use for TLS"
          <> value ".coop-publisher-cli/certificate.pem"
          <> showDefault
      )
    <*> strOption
      ( long "key-file"
          <> metavar "KEY_FILE"
          <> help "Private key file to use for TLS"
          <> value ".coop-publisher-cli/key.pem"
          <> showDefault
      )
    <*> strOption
      ( long "fact-statement-store-address"
          <> metavar "FS_STORE_ADDR"
          <> help "IP address or host name of the FactStatementStore gRpc service"
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "fact-statement-store-port"
          <> metavar "FS_STORE_PORT"
          <> help "TCP port of the FactStatementStore gRpc service"
          <> value 5082
          <> showDefault
      )
    <*> strOption
      ( long "tx-builder-address"
          <> metavar "TX_BUILDER_ADDR"
          <> help "IP address or host name of the TxBuilder gRpc service"
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "tx-builder-port"
          <> metavar "TX_STORE_PORT"
          <> help "TCP port of the TxBuilder gRpc service"
          <> value 5081
          <> showDefault
      )

optionsP :: Parser Command
optionsP =
  subparser $
    command
      "publisher-grpc"
      (info (PublisherGrpc <$> publisherGrpcOpts <* helper) (progDesc "Run a Publisher gRpc service"))

parserInfo :: ParserInfo Command
parserInfo = info (optionsP <**> helper) (fullDesc <> progDesc "COOP Publisher cli tools")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    PublisherGrpc opts -> publisherService opts
