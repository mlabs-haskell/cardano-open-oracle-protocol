{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Coop.Plutus.Types (
  PFsMpParams (..),
  PFsMpRedeemer (..),
  PFsVParams (..),
  PFsDatum (..),
  PCertDatum (..),
  PAuthParams (..),
) where

import Coop.Types (AuthParams, CertDatum, FsDatum, FsMpParams, FsMpRedeemer, FsVParams)
import Data.Typeable (Typeable)
import GHC.Generics qualified as GHC
import Generics.SOP (Generic)
import Plutarch (DerivePlutusType (DPTStrat))
import Plutarch.Api.V1 (
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
import Plutarch.Prelude (PAsData, PBool, PData, PDataRecord, PEq, PIsData, PLabeledType ((:=)), PTryFrom, PlutusType, S, Term)

-- TODO: Add Plutarch type plumbage for FactStatement
newtype PFsDatum s
  = PFsDatum
      ( Term
          s
          ( PDataRecord
              '[ "fd'fs" ':= PByteString
               , "fd'id" ':= PByteString
               , "fd'description" ':= PByteString
               , "fd'gcAfter" ':= PPOSIXTime
               , "fd'submitter" ':= PPubKeyHash
               , "fd'fsCs" ':= PCurrencySymbol
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
              '[ "fmp'coopInstance" ':= PCurrencySymbol
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

newtype PFsVParams s
  = PFsVParams
      ( Term
          s
          ( PDataRecord
              '[ "fvp'coopInstance" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (GHC.Generic, Typeable)
  deriving anyclass (Generic, PlutusType, PIsData, PEq, PDataFields)

instance DerivePlutusType PFsVParams where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PFsVParams where type PLifted PFsVParams = FsVParams
deriving via (DerivePConstantViaData FsVParams PFsVParams) instance (PConstantDecl FsVParams)
instance PTryFrom PData (PAsData PFsVParams)

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
               , "cert'cs" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType, PIsData, PEq, PTryFrom PData, PDataFields)

instance DerivePlutusType PCertDatum where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PCertDatum where type PLifted PCertDatum = CertDatum
deriving via (DerivePConstantViaData CertDatum PCertDatum) instance (PConstantDecl CertDatum)
instance PTryFrom PData (PAsData PCertDatum)

-- FIXME: Purge this when Plutarch supports it
instance PTryFrom PData (PAsData PBool)
instance PTryFrom PData (PExtended PPOSIXTime)
instance PTryFrom PData (PUpperBound PPOSIXTime)
instance PTryFrom PData (PLowerBound PPOSIXTime)
instance PTryFrom PData (PInterval PPOSIXTime)

instance PTryFrom PData (PAsData (PExtended PPOSIXTime))
instance PTryFrom PData (PAsData (PUpperBound PPOSIXTime))
instance PTryFrom PData (PAsData (PLowerBound PPOSIXTime))
instance PTryFrom PData (PAsData (PInterval PPOSIXTime))
