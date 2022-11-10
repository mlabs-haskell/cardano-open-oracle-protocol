module Main (main) where

import Control.Applicative ((<**>))
import FactStatementStoreGrpc (FactStatementStoreGrpcOpts (FactStatementStoreGrpcOpts), factStatementStoreService)
import Genesis (GenesisOpts (GenesisOpts), genesis)
import InsertFs (InsertFsOpts (InsertFsOpts), insertFs)
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
  = Genesis GenesisOpts
  | FactStatementStoreGrpc FactStatementStoreGrpcOpts
  | InsertFs InsertFsOpts

dbOpt :: Parser [Char]
dbOpt =
  strOption
    ( long "db"
        <> metavar "DB"
        <> help "SQLite database file location"
        <> value ".json-fs-store/json-store.db"
        <> showDefault
    )

genesisOpts :: Parser GenesisOpts
genesisOpts =
  GenesisOpts <$> dbOpt

fsStoreGrpcOpts :: Parser FactStatementStoreGrpcOpts
fsStoreGrpcOpts =
  FactStatementStoreGrpcOpts
    <$> dbOpt
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
          <> value ".json-fs-store/certificate.pem"
          <> showDefault
      )
    <*> strOption
      ( long "key-file"
          <> metavar "KEY_FILE"
          <> help "Private key file to use for TLS"
          <> value ".json-fs-store/key.pem"
          <> showDefault
      )

insertFsOpts :: Parser InsertFsOpts
insertFsOpts =
  InsertFsOpts
    <$> dbOpt
    <*> strOption
      ( long "fact_statement_id"
          <> metavar "FS_ID"
          <> help "Fact Statement ID to insert into the store"
      )
    <*> strOption
      ( long "json"
          <> metavar "FS_JSON"
          <> help "Fact Statement in a Json format to insert into the store"
      )

optionsP :: Parser Command
optionsP =
  subparser $
    command
      "fact-statement-store-grpc"
      (info (FactStatementStoreGrpc <$> fsStoreGrpcOpts <* helper) (progDesc "Run a FactStatementStore gRpc service"))
      <> command
        "genesis"
        (info (Genesis <$> genesisOpts <* helper) (progDesc "Initialise the service"))
      <> command
        "insert-fact-statement"
        (info (InsertFs <$> insertFsOpts <* helper) (progDesc "Insert a Fact Statement into the store"))

parserInfo :: ParserInfo Command
parserInfo = info (optionsP <**> helper) (fullDesc <> progDesc "JSON COOP Fact Statement Store cli tools")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    FactStatementStoreGrpc opts -> factStatementStoreService opts
    Genesis opts -> genesis opts
    InsertFs opts -> insertFs opts
