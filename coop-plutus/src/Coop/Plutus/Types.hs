{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Coop.Plutus.Types (
  PFsMpParams (..),
  PFsVParams (..),
  PFsDatum (..),
) where

import Coop.Types (FsMpParams, FsVParams)
import Data.Typeable (Typeable)
import GHC.Generics qualified as GHC
import Generics.SOP (Generic)
import Plutarch (DerivePlutusType (DPTStrat))
import Plutarch.Api.V1 (
  PAddress,
  PCurrencySymbol,
  PPOSIXTime,
  PPubKeyHash,
 )
import Plutarch.ByteString (PByteString)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
  PlutusTypeData,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude (PAsData, PData, PDataRecord, PEq, PIsData, PLabeledType ((:=)), PTryFrom, PlutusType, Term)

-- TODO: Add Plutarch type plumbage for FactStatement
newtype PFsDatum s
  = PFsDatum
      ( Term
          s
          ( PDataRecord
              '[ "fd'submittedBy" ':= PPubKeyHash
               , "fd'publishedBy" ':= PPubKeyHash
               , "fd'description" ':= PByteString
               , "fd'factStatement" ':= PByteString
               , "fd'gcAfter" ':= PPOSIXTime
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
