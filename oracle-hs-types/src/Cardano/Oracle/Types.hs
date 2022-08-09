{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Oracle.Types (CoopPlutus (..)) where

import Codec.Serialise (deserialise, serialise)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16S
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)

#ifdef NEW_LEDGER_NAMESPACE
import PlutusLedgerApi.V1 (Script)
#else
import Plutus.V1.Ledger.Api (Script)
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
