module Main (main) where

import Control.Applicative ((<**>))
import FactStatementStoreGrpc (FactStatementStoreGrpcOpts (FactStatementStoreGrpcOpts), factStatementStoreService)
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
  = FactStatementStoreGrpc FactStatementStoreGrpcOpts

configOptP :: Parser [Char]
configOptP =
  strOption
    ( long "config"
        <> metavar "CONFIG"
        <> help "Some config file"
        <> value "resources/config.yaml"
        <> showDefault
    )

fsStoreGrpcOpts :: Parser FactStatementStoreGrpcOpts
fsStoreGrpcOpts =
  FactStatementStoreGrpcOpts
    <$> configOptP
    <*> strOption
      ( long "address"
          <> metavar "ADDR"
          <> help "Local IP address or host name to bing the FactStatementStore gRpc service to"
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "port"
          <> metavar "PORT"
          <> help "TCP port to bind the FactStatementStore gRpc service to"
          <> value 5082
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
      "fact-statement-store-grpc"
      (info (FactStatementStoreGrpc <$> fsStoreGrpcOpts <* helper) (progDesc "Run a FactStatementStore gRpc service"))

parserInfo :: ParserInfo Command
parserInfo = info (optionsP <**> helper) (fullDesc <> progDesc "JSON COOP Fact Statement Store cli tools")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    FactStatementStoreGrpc opts -> factStatementStoreService opts
