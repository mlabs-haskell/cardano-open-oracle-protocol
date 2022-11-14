module Coop.Cli.Aux (pubKeyHashOpt, assetClassOpt, posixTimeOpt) where

import Coop.PlutusOrphans ()
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString (ByteString)
import Data.Hex (unhex)
import Data.String (fromString)
import Options.Applicative (Mod, OptionFields, Parser, auto, eitherReader, option)
import Plutus.V1.Ledger.Value (AssetClass)
import Plutus.V2.Ledger.Api (POSIXTime (POSIXTime), PubKeyHash (PubKeyHash), toBuiltin)

-- TODO: Implement support for all address formats
pubKeyHashOpt :: Mod OptionFields PubKeyHash -> Parser PubKeyHash
pubKeyHashOpt = option $ eitherReader (\s -> PubKeyHash . toBuiltin <$> unhex (fromString @ByteString s))

assetClassOpt :: Mod OptionFields AssetClass -> Parser AssetClass
assetClassOpt =
  option $
    eitherReader
      ( \s -> unhex (fromString @ByteString s) >>= eitherDecodeStrict @AssetClass
      )

posixTimeOpt :: Mod OptionFields POSIXTime -> Parser POSIXTime
posixTimeOpt = option $ POSIXTime <$> auto @Integer
