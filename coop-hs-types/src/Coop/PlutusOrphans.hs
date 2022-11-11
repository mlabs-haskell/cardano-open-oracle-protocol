{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-implicit-prelude #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Coop.PlutusOrphans () where

import Codec.Serialise (deserialise, serialise)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16S
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

#ifdef NEW_LEDGER_NAMESPACE
import PlutusLedgerApi.V2 (Script, LedgerBytes(LedgerBytes), PubKeyHash, CurrencySymbol, Address, BuiltinByteString, fromBuiltin, toBuiltin, Credential, StakingCredential, ValidatorHash, Validator, MintingPolicy, POSIXTime, POSIXTimeRange, LowerBound, UpperBound, Extended, TokenName(TokenName), TxOutRef, TxId, Data, BuiltinData (BuiltinData))
import PlutusLedgerApi.V1.Value (AssetClass)
#else
import Plutus.V2.Ledger.Api (Script, LedgerBytes(LedgerBytes), PubKeyHash, CurrencySymbol, Address, BuiltinByteString, fromBuiltin, toBuiltin, Credential, StakingCredential, ValidatorHash, Validator, MintingPolicy, POSIXTime, POSIXTimeRange, LowerBound, UpperBound, Extended, TokenName(TokenName), TxOutRef, TxId, Data, BuiltinData (BuiltinData))
import Plutus.V1.Ledger.Value (AssetClass)
#endif

instance ToJSON Script where
  toJSON = toJSON . toStrict . serialise

instance FromJSON Script where
  parseJSON json = deserialise . fromStrict <$> parseJSON json

instance ToJSON ByteString where
  toJSON = toJSON . decodeUtf8 . Base16S.encode

instance FromJSON ByteString where
  parseJSON (Aeson.String text) = either fail pure (Base16S.decode . encodeUtf8 $ text)
  parseJSON invalid =
    prependFailure
      "parsing ByteString failed, "
      (typeMismatch "base16 encoded bytes" invalid)

instance ToJSON MintingPolicy
instance FromJSON MintingPolicy

instance ToJSON Validator
instance FromJSON Validator

instance ToJSON PubKeyHash
instance FromJSON PubKeyHash

instance ToJSON Address
instance FromJSON Address

instance ToJSON CurrencySymbol
instance FromJSON CurrencySymbol

instance ToJSON Credential
instance FromJSON Credential

instance ToJSON StakingCredential
instance FromJSON StakingCredential

instance ToJSON ValidatorHash
instance FromJSON ValidatorHash

deriving newtype instance ToJSON LedgerBytes
deriving newtype instance FromJSON LedgerBytes

deriving newtype instance ToJSON TokenName
deriving newtype instance FromJSON TokenName

instance ToJSON POSIXTime
instance FromJSON POSIXTime

instance ToJSON POSIXTimeRange
instance FromJSON POSIXTimeRange

instance ToJSON (LowerBound POSIXTime)
instance FromJSON (LowerBound POSIXTime)

instance ToJSON (UpperBound POSIXTime)
instance FromJSON (UpperBound POSIXTime)

instance ToJSON (Extended POSIXTime)
instance FromJSON (Extended POSIXTime)

instance ToJSON BuiltinByteString where
  toJSON = toJSON . fromBuiltin @_ @ByteString

instance FromJSON BuiltinByteString where
  parseJSON v = toBuiltin <$> parseJSON @ByteString v

instance ToJSON AssetClass
instance FromJSON AssetClass

instance ToJSON TxOutRef
instance FromJSON TxOutRef

instance ToJSON TxId
instance FromJSON TxId

instance ToJSON Data
instance FromJSON Data

instance ToJSON BuiltinData where
  toJSON (BuiltinData d) = toJSON d

instance FromJSON BuiltinData where
  parseJSON v = BuiltinData <$> parseJSON @Data v
