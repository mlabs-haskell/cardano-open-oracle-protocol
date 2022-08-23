{-# LANGUAGE CPP #-}

module Coop.Types (
  CoopPlutus (..),
  CoopDeployment (..),
  FsMpParams (..),
  FsVParams (..),
  FsDescription (),
  FactStatement (),
  FsDatum (..),
  CertDatum (..),
) where

import Coop.PlutusOrphans ()
import Data.Aeson (FromJSON, ToJSON)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import PlutusTx qualified

#ifdef NEW_LEDGER_NAMESPACE
import PlutusLedgerApi.V1 (Script, LedgerBytes, CurrencySymbol, Address, Validator, MintingPolicy, POSIXTime, POSIXTimeRange)
#else
import Plutus.V1.Ledger.Api (Script, LedgerBytes, CurrencySymbol, Address, Validator, MintingPolicy, POSIXTime, POSIXTimeRange)
#endif

data CoopPlutus = CoopPlutus
  { cp'mkCoopInstanceMp :: Script
  , cp'mkFsMp :: Script
  , cp'mkFsV :: Script
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CoopDeployment = CoopDeployment
  { cd'fsMpParams :: FsMpParams
  , cd'fsMp :: MintingPolicy
  , cd'fsVParams :: FsVParams
  , cd'fsV :: Validator
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Plutus types
type FsDescription = LedgerBytes
type FactStatement = LedgerBytes

data FsDatum = FsDatum
  { fd'fs :: FactStatement
  , fd'description :: FsDescription
  , fs'gcAfter :: POSIXTime
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

data FsMpParams = FsMpParams
  { fmp'coopInstance :: CurrencySymbol -- provided by the one shot mp,
  , fmp'fsVAddress :: Address
  , fmp'authTokenCs :: CurrencySymbol
  }
  deriving stock (Show, Generic, Eq, Typeable)
  deriving anyclass (ToJSON, FromJSON)

newtype FsVParams = FsVParams
  { fvp'coopInstance :: CurrencySymbol -- provided by the one shot mp
  }
  deriving stock (Show, Generic, Eq, Typeable)
  deriving anyclass (ToJSON, FromJSON)

-- Authentication Tokens and Certificates

data CertDatum = CertDatum
  { cd'id :: LedgerBytes
  , cd'validity :: POSIXTimeRange
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''FsMpParams
PlutusTx.unstableMakeIsData ''FsVParams
PlutusTx.unstableMakeIsData ''FsDatum
PlutusTx.unstableMakeIsData ''CertDatum
