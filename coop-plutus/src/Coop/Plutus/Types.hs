{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Coop.Plutus.Types (
  PFsMpParams (..),
  PFsVParams (..),
  PFsDatum (..),
  PCertDatum (..),
) where

import Coop.Types (CertDatum, FsMpParams, FsVParams)
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
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType, PIsData, PEq, PTryFrom PData, PDataFields)

instance DerivePlutusType PFsDatum where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData PFsDatum)

newtype PFsMpParams s
  = PFsMpParams
      ( Term
          s
          ( PDataRecord
              '[ "fmp'coopInstance" ':= PCurrencySymbol
               , "fmp'fsVAddress" ':= PAddress
               , "fmp'authTokenCs" ':= PCurrencySymbol
               , "fmp'certTokenCs" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (GHC.Generic, Typeable)
  deriving anyclass (Generic, PlutusType, PIsData, PEq, PDataFields)

instance DerivePlutusType PFsMpParams where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PFsMpParams where type PLifted PFsMpParams = FsMpParams
deriving via (DerivePConstantViaData FsMpParams PFsMpParams) instance (PConstantDecl FsMpParams)

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

newtype PCertDatum (s :: S)
  = PCertDatum
      ( Term
          s
          ( PDataRecord
              '[ "cert'id" ':= PByteString
               , "cert'validity" ':= PPOSIXTimeRange
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType, PIsData, PEq, PTryFrom PData, PDataFields)

instance DerivePlutusType PCertDatum where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData PCertDatum)
instance PUnsafeLiftDecl PCertDatum where type PLifted PCertDatum = CertDatum
deriving via (DerivePConstantViaData CertDatum PCertDatum) instance (PConstantDecl CertDatum)

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
