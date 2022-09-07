{-# LANGUAGE CPP #-}

module Coop.Types (
  CoopPlutus (..),
  CoopDeployment (..),
  FsMpParams (..),
  FsMpRedeemer (..),
  FsVParams (..),
  FsDescription (),
  FactStatement (),
  FsDatum (..),
  CertDatum (..),
  AuthParams (..),
  AuthMpParams (..),
  AuthMpRedeemer (..),
  CertMpParams (..),
  CertMpRedeemer (..),
  AuthDeployment (..),
) where

import Control.Lens (makeFields)
import Coop.PlutusOrphans ()
import Data.Aeson (FromJSON, ToJSON)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import PlutusTx qualified

#ifdef NEW_LEDGER_NAMESPACE
import PlutusLedgerApi.V1 (Script, LedgerBytes, CurrencySymbol, Address, Validator, MintingPolicy, POSIXTime, POSIXTimeRange, PubKeyHash, TokenName)
#else
import Plutus.V1.Ledger.Api (Script, LedgerBytes, CurrencySymbol, Address, Validator, MintingPolicy, POSIXTime, POSIXTimeRange, PubKeyHash, TokenName)
#endif

data CoopPlutus = CoopPlutus
  { cp'mkNftMp :: Script
  , cp'mkAuthMp :: Script
  , cp'mkCertMp :: Script
  , cp'certV :: Script
  , cp'mkFsMp :: Script
  , cp'mkFsV :: Script
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CoopDeployment = CoopDeployment
  { cd'coopToken :: (CurrencySymbol, TokenName)
  , cd'fsMp :: MintingPolicy
  , cd'fsV :: Validator
  , cd'auth :: AuthDeployment
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Plutus types
type FsDescription = LedgerBytes

type FactStatement = LedgerBytes

data FsDatum = FsDatum
  { fd'fs :: FactStatement
  , fd'id :: LedgerBytes
  , fd'description :: FsDescription
  , fs'gcAfter :: POSIXTime
  , fs'submitter :: PubKeyHash
  , fs'fsCs :: CurrencySymbol
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

data FsMpParams = FsMpParams
  { fmp'coopInstance :: CurrencySymbol -- provided by the one shot mp,
  , fmp'fsVAddress :: Address
  , fmp'authParams :: AuthParams
  }
  deriving stock (Show, Generic, Eq, Typeable)
  deriving anyclass (ToJSON, FromJSON)

data FsMpRedeemer = FsMpBurn | FsMpMint
  deriving stock (Show, Generic, Eq, Typeable)
  deriving anyclass (ToJSON, FromJSON)

newtype FsVParams = FsVParams
  { fvp'coopInstance :: CurrencySymbol -- provided by the one shot mp
  }
  deriving stock (Show, Generic, Eq, Typeable)
  deriving anyclass (ToJSON, FromJSON)

-- | Authentication Tokens and Certificates
data AuthDeployment = AuthDeployment
  { ad'authorityToken :: (CurrencySymbol, TokenName)
  , ad'certV :: Validator
  , ad'certMp :: MintingPolicy
  , ad'authMp :: MintingPolicy
  }
  deriving stock (Show, Generic, Eq, Typeable)
  deriving anyclass (ToJSON, FromJSON)

data AuthParams = AuthParams
  { ap'authTokenCs :: CurrencySymbol
  , ap'certTokenCs :: CurrencySymbol
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

data CertDatum = CertDatum
  { cert'id :: LedgerBytes
  , cert'validity :: POSIXTimeRange
  , cert'redeemerAc :: (CurrencySymbol, TokenName)
  , cert'cs :: CurrencySymbol -- TODO: remove
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

data CertMpRedeemer = CertMpBurn | CertMpMint
  deriving stock (Show, Generic, Eq, Typeable)
  deriving anyclass (ToJSON, FromJSON)

data CertMpParams = CertMpParams
  { cmp'authAuthorityAc :: (CurrencySymbol, TokenName)
  , cmp'authAuthorityQ :: Integer
  , cmp'certVAddress :: Address
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

data AuthMpRedeemer = AuthMpBurn | AuthMpMint
  deriving stock (Show, Generic, Eq, Typeable)
  deriving anyclass (ToJSON, FromJSON)

data AuthMpParams = AuthMpParams
  { amp'authAuthorityAc :: (CurrencySymbol, TokenName)
  , amp'authAuthorityQ :: Integer
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

-- | Plutus ToData/FromData instances
PlutusTx.unstableMakeIsData ''CertDatum
PlutusTx.unstableMakeIsData ''AuthParams
PlutusTx.unstableMakeIsData ''CertMpParams
PlutusTx.unstableMakeIsData ''CertMpRedeemer
PlutusTx.unstableMakeIsData ''AuthMpParams
PlutusTx.unstableMakeIsData ''AuthMpRedeemer

PlutusTx.unstableMakeIsData ''FsMpParams
PlutusTx.unstableMakeIsData ''FsVParams
PlutusTx.unstableMakeIsData ''FsDatum
PlutusTx.unstableMakeIsData ''FsMpRedeemer

-- | Lenses
makeFields ''CoopPlutus
makeFields ''CoopDeployment
makeFields ''FsMpParams
makeFields ''FsVParams
makeFields ''FsDatum
makeFields ''FsMpRedeemer

makeFields ''AuthDeployment
makeFields ''CertDatum
makeFields ''AuthParams
makeFields ''CertMpParams
makeFields ''CertMpRedeemer
makeFields ''AuthMpParams
makeFields ''AuthMpRedeemer
