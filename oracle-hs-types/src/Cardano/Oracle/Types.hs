{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Oracle.Types (CoopPlutus (..), ResourceMintingParams (..), ResourceDescription, Resource, ResourceDatum) where

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
import PlutusLedgerApi.V1 (Script, LedgerBytes, PubKeyHash, CurrencySymbol, Address)
#else
import Plutus.V1.Ledger.Api (Script, LedgerBytes, PubKeyHash, CurrencySymbol, Address)
#endif

data CoopPlutus = CoopPlutus
  { cp'instanceMintingPolicy :: Script
  , cp'resourceMintingPolicy :: Script
  , cp'resourceValidator :: Script
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CoopPlutus
instance FromJSON CoopPlutus

instance ToJSON Script where
  toJSON = toJSON . toStrict . serialise

instance ToJSON ByteString where
  toJSON = toJSON . decodeUtf8 . Base16S.encode

instance FromJSON Script where
  parseJSON json = deserialise . fromStrict <$> parseJSON json

instance FromJSON ByteString where
  parseJSON (Aeson.String text) = either fail pure (Base16S.decode . encodeUtf8 $ text)
  parseJSON invalid =
    prependFailure
      "parsing ByteString failed, "
      (typeMismatch "base16 encoded bytes" invalid)

-- Plutus types
type ResourceDescription = LedgerBytes
type Resource = LedgerBytes

data ResourceDatum = ResourceDatum
  { submittedBy :: PubKeyHash
  , publishedBy :: PubKeyHash
  , description :: ResourceDescription
  , resource :: Resource
  }
  deriving stock (Show, Generic, Eq)

data ResourceMintingParams = ResourceMintingParams
  { rmp'instanceCs :: CurrencySymbol -- provided by the one shot mp,
  , rmp'resourceValidatorAddress :: Address
  }
  deriving stock (Show, Generic, Eq, Typeable)

PlutusTx.unstableMakeIsData ''ResourceMintingParams
