{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Coop.Types (
  CoopPlutus (..),
  CoopDeployment (..),
  SofMpParams (..),
  SofVParams (..),
  SofDescription (),
  Sof (),
  SofDatum (..),
) where

import Codec.Serialise (deserialise, serialise)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16S
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import PlutusTx qualified

#ifdef NEW_LEDGER_NAMESPACE
import PlutusLedgerApi.V1 (Script, LedgerBytes(LedgerBytes), PubKeyHash, CurrencySymbol, Address, BuiltinByteString, fromBuiltin, toBuiltin, Credential, StakingCredential, ValidatorHash, Validator, MintingPolicy)
#else
import Plutus.V1.Ledger.Api (Script, LedgerBytes(LedgerBytes), PubKeyHash, CurrencySymbol, Address, BuiltinByteString, fromBuiltin, toBuiltin, Credential, StakingCredential, ValidatorHash, Validator, MintingPolicy)
#endif

data CoopPlutus = CoopPlutus
  { cp'mkCoopInstanceMp :: Script
  , cp'mkSofMp :: Script
  , cp'mkSofV :: Script
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CoopDeployment = CoopDeployment
  { cd'sofMpParams :: SofMpParams
  , cd'sofMp :: MintingPolicy
  , cd'sofVParams :: SofVParams
  , cd'sofV :: Validator
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Plutus types
type SofDescription = LedgerBytes
type Sof = LedgerBytes

data SofDatum = SofDatum
  { sd'submittedBy :: PubKeyHash
  , sd'publishedBy :: PubKeyHash
  , sd'description :: SofDescription
  , sd'sof :: Sof
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

data SofMpParams = SofMpParams
  { smp'coopInstance :: CurrencySymbol -- provided by the one shot mp,
  , smp'sofVAddress :: Address
  }
  deriving stock (Show, Generic, Eq, Typeable)
  deriving anyclass (ToJSON, FromJSON)

newtype SofVParams = SofVParams
  { svp'coopInstance :: CurrencySymbol -- provided by the one shot mp
  }
  deriving stock (Show, Generic, Eq, Typeable)
  deriving anyclass (ToJSON, FromJSON)

-- Missing instances
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

instance ToJSON BuiltinByteString where
  toJSON = toJSON . fromBuiltin @_ @ByteString

instance FromJSON BuiltinByteString where
  parseJSON v = toBuiltin <$> parseJSON @ByteString v

PlutusTx.unstableMakeIsData ''SofMpParams
PlutusTx.unstableMakeIsData ''SofVParams
PlutusTx.unstableMakeIsData ''SofDatum
