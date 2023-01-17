{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Applicative (Alternative (many), (<**>))

import Codec.Serialise (deserialise, deserialiseOrFail)
import Data.Aeson (decode, decodeStrict', encodeFile, json)
import Data.Aeson.Parser (decodeStrictWith)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as LB
import Data.Maybe (fromMaybe)
import Options.Applicative (
  Parser,
  ParserInfo,
  command,
  customExecParser,
  flag,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  prefs,
  progDesc,
  short,
  showDefault,
  showHelpOnEmpty,
  showHelpOnError,
  strOption,
  subparser,
 )
import PlutusJson (jsonToPlutusData, plutusDataToJson)
import PlutusTx (Data, ToData (toBuiltinData))
import PlutusTx.Builtins (dataToBuiltinData, fromBuiltin, serialiseData, toBuiltin)

data Command
  = ToJson FilePath FilePath
  | FromJson FilePath FilePath

toJsonOptsP :: Parser Command
toJsonOptsP =
  ToJson
    <$> strOption
      ( long "in"
          <> short 'i'
          <> metavar "FILEPATH"
          <> help "PlutusData file (CBOR encoded) to translate into a Json file"
      )
    <*> strOption
      ( long "out"
          <> short 'o'
          <> metavar "FILEPATH"
          <> help "Translated Json file"
      )

fromJsonOptsP :: Parser Command
fromJsonOptsP =
  FromJson
    <$> strOption
      ( long "in"
          <> short 'i'
          <> metavar "FILEPATH"
          <> help "Json file to translate into a PlutusData file (CBOR encoded)"
      )
    <*> strOption
      ( long "out"
          <> short 'o'
          <> metavar "FILEPATH"
          <> help "Translated PlutusData file (CBOR encoded)"
      )

optionsP :: Parser Command
optionsP =
  subparser $
    command
      "to-json"
      (info (toJsonOptsP <* helper) (progDesc "Translate a PlutusData file (CBOR encoded) into a Json file"))
      <> command
        "from-json"
        (info (fromJsonOptsP <* helper) (progDesc "Translate a Json file into a PlutusData file (CBOR encoded)"))

parserInfo :: ParserInfo Command
parserInfo = info (optionsP <**> helper) (fullDesc <> progDesc "COOP plutus-json")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    ToJson inf outf -> do
      cborBytes <- LB.readFile inf
      let errOrDecoded = deserialiseOrFail @Data cborBytes
      plData <- either (\err -> error $ "File " <> inf <> " can't be parsed into PlutusData CBOR: " <> show err) return errOrDecoded
      jsVal <- plutusDataToJson plData
      encodeFile outf jsVal
    FromJson inf outf -> do
      jsonBytes <- B.readFile inf
      let mayDecoded = decodeStrictWith json return jsonBytes
      decoded <- maybe (error $ "File " <> inf <> " can't be parsed into Json") return mayDecoded
      let plData = jsonToPlutusData decoded
      B.writeFile outf (fromBuiltin . serialiseData . dataToBuiltinData $ plData)
      return ()
