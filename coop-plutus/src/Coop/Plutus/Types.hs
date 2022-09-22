{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Coop.Plutus.Types (
  PFsMpParams (..),
  PFsMpRedeemer (..),
  PFsDatum (..),
  PCertDatum (..),
  PAuthParams (..),
  PAuthMpParams (..),
  PAuthMpRedeemer (..),
  PCertMpParams (..),
  PCertMpRedeemer (..),
) where

import Coop.Types (AuthMpParams, AuthMpRedeemer, AuthParams, CertDatum, CertMpParams, CertMpRedeemer, FsDatum, FsMpParams, FsMpRedeemer)
import Data.Typeable (Typeable)
import GHC.Generics qualified as GHC
import Generics.SOP (Generic)
import Plutarch (DerivePlutusType (DPTStrat))
import Plutarch.Api.V2 (
  PAddress,
  PCurrencySymbol,
  PExtended,
  PInterval,
  PLowerBound,
  PPOSIXTime,
  PPOSIXTimeRange,
  PPubKeyHash,
  PTokenName,
  PTuple,
  PUpperBound,
 )
import Plutarch.ByteString (PByteString)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
  PlutusTypeData,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude (PAsData, PBool, PData, PDataRecord, PEq, PInteger, PIsData, PLabeledType ((:=)), PTryFrom, PlutusType, S, Term)
import PlutusLedgerApi.V1.Value (AssetClass)

newtype PFsDatum s
  = PFsDatum
      ( Term
          s
          ( PDataRecord
              '[ "fd'fs" ':= PData
               , "fd'fsId" ':= PByteString
               , "fd'gcAfter" ':= PExtended PPOSIXTime
               , "fd'submitter" ':= PPubKeyHash
               ]
          )
      )
  deriving stock (GHC.Generic, Typeable)
  deriving anyclass (Generic, PlutusType, PIsData, PEq, PTryFrom PData, PDataFields)

instance DerivePlutusType PFsDatum where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PFsDatum where type PLifted PFsDatum = FsDatum
deriving via (DerivePConstantViaData FsDatum PFsDatum) instance (PConstantDecl FsDatum)
instance PTryFrom PData (PAsData PFsDatum)

data PFsMpRedeemer s
  = PFsMpBurn (Term s (PDataRecord '[]))
  | PFsMpMint (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic, Typeable)
  deriving anyclass (Generic, PlutusType, PIsData, PEq)

instance DerivePlutusType PFsMpRedeemer where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PFsMpRedeemer where type PLifted PFsMpRedeemer = FsMpRedeemer
deriving via (DerivePConstantViaData FsMpRedeemer PFsMpRedeemer) instance (PConstantDecl FsMpRedeemer)
instance PTryFrom PData (PAsData PFsMpRedeemer)

newtype PFsMpParams s
  = PFsMpParams
      ( Term
          s
          ( PDataRecord
              '[ "fmp'coopAc" ':= PTuple PCurrencySymbol PTokenName
               , "fmp'fsVAddress" ':= PAddress
               , "fmp'authParams" ':= PAuthParams
               ]
          )
      )
  deriving stock (GHC.Generic, Typeable)
  deriving anyclass (Generic, PlutusType, PIsData, PEq, PDataFields)

instance DerivePlutusType PFsMpParams where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PFsMpParams where type PLifted PFsMpParams = FsMpParams
deriving via (DerivePConstantViaData FsMpParams PFsMpParams) instance (PConstantDecl FsMpParams)
instance PTryFrom PData (PAsData PFsMpParams)

newtype PAuthParams s
  = PAuthParams
      ( Term
          s
          ( PDataRecord
              '[ "ap'authTokenCs" ':= PCurrencySymbol
               , "ap'certTokenCs" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (GHC.Generic, Typeable)
  deriving anyclass (Generic, PlutusType, PIsData, PEq, PTryFrom PData, PDataFields)

instance DerivePlutusType PAuthParams where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PAuthParams where type PLifted PAuthParams = AuthParams
deriving via (DerivePConstantViaData AuthParams PAuthParams) instance (PConstantDecl AuthParams)
instance PTryFrom PData (PAsData PAuthParams)

newtype PCertDatum (s :: S)
  = PCertDatum
      ( Term
          s
          ( PDataRecord
              '[ "cert'id" ':= PByteString
               , "cert'validity" ':= PPOSIXTimeRange
               , "cert'redeemerAc" ':= PTuple PCurrencySymbol PTokenName
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType, PIsData, PEq, PTryFrom PData, PDataFields)

instance DerivePlutusType PCertDatum where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PCertDatum where type PLifted PCertDatum = CertDatum
deriving via (DerivePConstantViaData CertDatum PCertDatum) instance (PConstantDecl CertDatum)
instance PTryFrom PData (PAsData PCertDatum)

newtype PAuthMpParams (s :: S)
  = PAuthMpParams
      ( Term
          s
          ( PDataRecord
              '[ "amp'authAuthorityAc" ':= PTuple PCurrencySymbol PTokenName
               , "amp'requiredAtLeastAaQ" ':= PInteger
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType, PIsData, PEq, PTryFrom PData, PDataFields)

instance DerivePlutusType PAuthMpParams where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PAuthMpParams where type PLifted PAuthMpParams = AuthMpParams
deriving via (DerivePConstantViaData AuthMpParams PAuthMpParams) instance (PConstantDecl AuthMpParams)
instance PTryFrom PData (PAsData PAuthMpParams)

data PAuthMpRedeemer s
  = PAuthMpBurn (Term s (PDataRecord '[]))
  | PAuthMpMint (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic, Typeable)
  deriving anyclass (Generic, PlutusType, PIsData, PEq)

instance DerivePlutusType PAuthMpRedeemer where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PAuthMpRedeemer where type PLifted PAuthMpRedeemer = AuthMpRedeemer
deriving via (DerivePConstantViaData AuthMpRedeemer PAuthMpRedeemer) instance (PConstantDecl AuthMpRedeemer)
instance PTryFrom PData (PAsData PAuthMpRedeemer)

newtype PCertMpParams (s :: S)
  = PCertMpParams
      ( Term
          s
          ( PDataRecord
              '[ "cmp'authAuthorityAc" ':= PTuple PCurrencySymbol PTokenName
               , "cmp'requiredAtLeastAaQ" ':= PInteger
               , "cmp'certVAddress" ':= PAddress
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType, PIsData, PEq, PTryFrom PData, PDataFields)

instance DerivePlutusType PCertMpParams where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PCertMpParams where type PLifted PCertMpParams = CertMpParams
deriving via (DerivePConstantViaData CertMpParams PCertMpParams) instance (PConstantDecl CertMpParams)
instance PTryFrom PData (PAsData PCertMpParams)

data PCertMpRedeemer s
  = PCertMpBurn (Term s (PDataRecord '[]))
  | PCertMpMint (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic, Typeable)
  deriving anyclass (Generic, PlutusType, PIsData, PEq)

instance DerivePlutusType PCertMpRedeemer where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PCertMpRedeemer where type PLifted PCertMpRedeemer = CertMpRedeemer
deriving via (DerivePConstantViaData CertMpRedeemer PCertMpRedeemer) instance (PConstantDecl CertMpRedeemer)
instance PTryFrom PData (PAsData PCertMpRedeemer)

-- FIXME: Purge this when Plutarch supports it
instance PUnsafeLiftDecl (PTuple PCurrencySymbol PTokenName) where type PLifted (PTuple PCurrencySymbol PTokenName) = AssetClass
deriving via (DerivePConstantViaData AssetClass (PTuple PCurrencySymbol PTokenName)) instance (PConstantDecl AssetClass)

instance PTryFrom PData (PAsData PBool)
instance PTryFrom PData (PExtended PPOSIXTime)
instance PTryFrom PData (PUpperBound PPOSIXTime)
instance PTryFrom PData (PLowerBound PPOSIXTime)
instance PTryFrom PData (PInterval PPOSIXTime)

instance PTryFrom PData (PAsData (PExtended PPOSIXTime))
instance PTryFrom PData (PAsData (PUpperBound PPOSIXTime))
instance PTryFrom PData (PAsData (PLowerBound PPOSIXTime))
instance PTryFrom PData (PAsData (PInterval PPOSIXTime))
