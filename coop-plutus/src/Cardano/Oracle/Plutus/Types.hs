{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Oracle.Plutus.Types (
  PResourceMintingParams (..),
  PResourceValidatorParams (..),
  PResourceDatum (..),
) where

import Cardano.Oracle.Types (ResourceMintingParams, ResourceValidatorParams)
import Data.Typeable (Typeable)
import GHC.Generics qualified as GHC
import Generics.SOP (Generic)
import Plutarch (DerivePlutusType (DPTStrat))
import Plutarch.Api.V1 (
  PAddress,
  PCurrencySymbol (PCurrencySymbol),
  PPubKeyHash (PPubKeyHash),
 )
import Plutarch.Bool ((#||))
import Plutarch.ByteString (PByteString, plengthBS)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
  PlutusTypeData,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude (PAsData, PData, PDataRecord, PEq ((#==)), PIsData, PLabeledType ((:=)), PTryFrom, PlutusType, Term, pcon, pif, plet, ptraceError, ptryFrom, (#))
import Plutarch.TermCont (TermCont (runTermCont), tcont)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)
import Prelude (Applicative (pure), snd, ($), (.))

newtype PResourceDatum s
  = PResourceDatum
      ( Term
          s
          ( PDataRecord
              '[ "submittedBy" ':= PPubKeyHash
               , "publishedBy" ':= PPubKeyHash
               , "description" ':= PByteString
               , "resource" ':= PByteString
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType, PIsData, PEq, PTryFrom PData, PDataFields)

instance DerivePlutusType PResourceDatum where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData PResourceDatum)

-- FIXME: Integrate https://github.com/Plutonomicon/plutarch-plutus/pull/520
instance PTryFrom PData (PAsData PPubKeyHash) where
  type PTryFromExcess PData (PAsData PPubKeyHash) = Flip Term PPubKeyHash
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    tcont $ \f -> pif (plengthBS # unwrapped #== 28) (f ()) (ptraceError "a PubKeyHash must be 28 bytes long")
    pure (punsafeCoerce opq, pcon . PPubKeyHash $ unwrapped)

newtype Flip f a b = Flip (f b a) deriving stock (GHC.Generic)

newtype PResourceMintingParams s
  = PResourceMintingParams
      ( Term
          s
          ( PDataRecord
              '[ "rmp'instanceCs" ':= PCurrencySymbol
               , "rmp'resourceValidatorAddress" ':= PAddress
               ]
          )
      )
  deriving stock (GHC.Generic, Typeable)
  deriving anyclass (Generic, PlutusType, PIsData, PEq, PDataFields)

instance DerivePlutusType PResourceMintingParams where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PResourceMintingParams where type PLifted PResourceMintingParams = ResourceMintingParams
deriving via (DerivePConstantViaData ResourceMintingParams PResourceMintingParams) instance (PConstantDecl ResourceMintingParams)

instance PTryFrom PData (PAsData PCurrencySymbol) where
  type PTryFromExcess PData (PAsData PCurrencySymbol) = Flip Term PCurrencySymbol
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    len <- tcont . plet $ plengthBS # unwrapped
    tcont $ \f ->
      pif (len #== 0 #|| len #== 28) (f ()) (ptraceError "a CurrencySymbol must be 28 bytes long or empty")
    pure (punsafeCoerce opq, pcon . PCurrencySymbol $ unwrapped)

newtype PResourceValidatorParams s
  = PResourceValidatorParams
      ( Term
          s
          ( PDataRecord
              '[ "rvp'InstanceCs" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (GHC.Generic, Typeable)
  deriving anyclass (Generic, PlutusType, PIsData, PEq, PDataFields)

instance DerivePlutusType PResourceValidatorParams where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PResourceValidatorParams where type PLifted PResourceValidatorParams = ResourceValidatorParams
deriving via (DerivePConstantViaData ResourceValidatorParams PResourceValidatorParams) instance (PConstantDecl ResourceValidatorParams)
