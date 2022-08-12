{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Coop.Plutus.Types (
  PSofMpParams (..),
  PSofVParams (..),
  PSofDatum (..),
) where

import Coop.Types (SofMpParams, SofVParams)
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

newtype PSofDatum s
  = PSofDatum
      ( Term
          s
          ( PDataRecord
              '[ "sd'submittedBy" ':= PPubKeyHash
               , "sd'publishedBy" ':= PPubKeyHash
               , "sd'description" ':= PByteString
               , "sd'sof" ':= PByteString
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType, PIsData, PEq, PTryFrom PData, PDataFields)

instance DerivePlutusType PSofDatum where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData PSofDatum)

-- FIXME: Integrate https://github.com/Plutonomicon/plutarch-plutus/pull/520
instance PTryFrom PData (PAsData PPubKeyHash) where
  type PTryFromExcess PData (PAsData PPubKeyHash) = Flip Term PPubKeyHash
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    tcont $ \f -> pif (plengthBS # unwrapped #== 28) (f ()) (ptraceError "a PubKeyHash must be 28 bytes long")
    pure (punsafeCoerce opq, pcon . PPubKeyHash $ unwrapped)

newtype Flip f a b = Flip (f b a) deriving stock (GHC.Generic)

newtype PSofMpParams s
  = PSofMpParams
      ( Term
          s
          ( PDataRecord
              '[ "smp'coopInstance" ':= PCurrencySymbol
               , "smp'sofVAddress" ':= PAddress
               ]
          )
      )
  deriving stock (GHC.Generic, Typeable)
  deriving anyclass (Generic, PlutusType, PIsData, PEq, PDataFields)

instance DerivePlutusType PSofMpParams where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PSofMpParams where type PLifted PSofMpParams = SofMpParams
deriving via (DerivePConstantViaData SofMpParams PSofMpParams) instance (PConstantDecl SofMpParams)

instance PTryFrom PData (PAsData PCurrencySymbol) where
  type PTryFromExcess PData (PAsData PCurrencySymbol) = Flip Term PCurrencySymbol
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    len <- tcont . plet $ plengthBS # unwrapped
    tcont $ \f ->
      pif (len #== 0 #|| len #== 28) (f ()) (ptraceError "a CurrencySymbol must be 28 bytes long or empty")
    pure (punsafeCoerce opq, pcon . PCurrencySymbol $ unwrapped)

newtype PSofVParams s
  = PSofVParams
      ( Term
          s
          ( PDataRecord
              '[ "svp'coopInstance" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (GHC.Generic, Typeable)
  deriving anyclass (Generic, PlutusType, PIsData, PEq, PDataFields)

instance DerivePlutusType PSofVParams where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PSofVParams where type PLifted PSofVParams = SofVParams
deriving via (DerivePConstantViaData SofVParams PSofVParams) instance (PConstantDecl SofVParams)
