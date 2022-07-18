{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Oracle.Plutus (exampleMintingPolicy, exampleValidator, ptxOutValue, ptxSignedBy) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic)
import Plutarch (Config, DerivePlutusType (DPTStrat), POpaque, popaque, pto)
import Plutarch.Api.V1 (
  PCurrencySymbol,
  PDatum,
  PDatumHash,
  PMaybeData (PDJust, PDNothing),
  PScriptContext,
  PScriptPurpose (PMinting),
  PTxInfo (PTxInfo),
  PTxOut (PTxOut),
  PValue,
  mkMintingPolicy,
  mkValidator,
 )
import Plutarch.Api.V1.Crypto (PPubKeyHash)
import Plutarch.Api.V1.Value (pnormalize)
import Plutarch.Api.V1.Value qualified as PValue
import Plutarch.Bool (PBool (PTrue))
import Plutarch.DataRepr (
  PlutusTypeData,
 )
import Plutarch.Extra.TermCont (pmatchC, ptraceC)
import Plutarch.Prelude (
  PAsData,
  PBool (PFalse),
  PBuiltinList,
  PData,
  PDataRecord,
  PEq ((#==)),
  PIsData,
  PLabeledType ((:=)),
  PMaybe (PJust, PNothing),
  PString,
  PTryFrom,
  PlutusType,
  Term,
  pcon,
  pconstant,
  pfield,
  pfind,
  pfromData,
  phoistAcyclic,
  plam,
  plet,
  pmatch,
  ptraceError,
  ptryFrom,
  (#),
  (#$),
  type (:-->),
 )
import Plutarch.TermCont (TermCont, tcont, unTermCont)
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Scripts (MintingPolicy, Validator)
import Prelude (Applicative (pure), String, fst, ($), (.), (<$>))
import Prelude qualified as Hask

pletC :: Term s a -> TermCont s (Term s a)
pletC = tcont . plet

type ResourceDescription = String
type Resource = String

data ResourceDatum = ResourceDatum
  { submittedBy :: PubKeyHash
  , publishedBy :: PubKeyHash
  , description :: ResourceDescription
  , resource :: Resource
  }
  deriving stock (Hask.Show, GHC.Generic, Hask.Eq)

data PResourceDatum s
  = PResourceDatum
      ( Term
          s
          ( PDataRecord
              '[ "submittedBy" ':= PPubKeyHash
               , "publishedBy" ':= PPubKeyHash
               , "description" ':= PString
               , "resource" ':= PString
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType, PIsData, PEq)

instance DerivePlutusType PResourceDatum where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData PResourceDatum)

instance PTryFrom PData (PAsData POpaque)
instance PTryFrom PData (PAsData PString)
instance PTryFrom PData (PAsData PPubKeyHash)

exampleValidator :: Config -> Validator
exampleValidator cfg = mkValidator cfg $ plam $ \_ _ _ -> popaque $ pconstant ()

exampleMintingPolicy :: Config -> MintingPolicy
exampleMintingPolicy cfg = mkMintingPolicy cfg $
  plam $ \_ ctx -> unTermCont do
    info <- pletC $ pscriptContextTxInfo # ctx
    _ownCs <- pletC $ pownCurrencySymbol # ctx
    _minted <- pletC $ ptxInfoMint # info
    -- resourceAssetMinted <- pletC $ pvalueOf # minted # ownCs # (PValue.PTokenName # "asd")
    -- resourceAssetSent <- pletC $ pvalueOf # minted # ownCs # (PValue.PTokenName # "asd")
    _ <- pletC $ findOutputWithResource # ctx
    pure $ popaque $ pconstant ()

findOutputWithResource :: Term s (PScriptContext :--> PMaybe PTxOut)
findOutputWithResource = phoistAcyclic $
  plam $ \ctx -> unTermCont $ do
    ptraceC "findOutputWithResource"
    _ownCs <- pletC $ pownCurrencySymbol # ctx
    txInfo <- pletC $ pscriptContextTxInfo # ctx
    findDatum' <- pletC $ pfindDatum # txInfo
    txOuts <- pletC $ ptxInfoOutputs # txInfo
    pure $
      pfind
        # plam
          ( \out -> pmatch (parseOutputWithResource # findDatum' # out) \case
              PNothing -> pcon PFalse
              PJust _ -> pcon PTrue
          )
        #$ txOuts

parseOutputWithResource :: Term s ((PDatumHash :--> PMaybe PDatum) :--> PTxOut :--> PMaybe PResourceDatum)
parseOutputWithResource = phoistAcyclic $
  plam $ \findDatum txOut -> unTermCont $ do
    ptraceC "parseOutputWithResource"
    PTxOut to <- pmatchC txOut
    _outVal <- pletC $ pfield @"value" # to
    _outAddr <- pletC $ pfield @"address" # to
    mayOutDatumHash <- pletC $ pfield @"datumHash" # to
    pure $ pmatch mayOutDatumHash \case
      PDNothing _ -> unTermCont $ pure $ pcon PNothing
      PDJust outDatumHash -> unTermCont $ do
        odh <- pletC $ pfield @"_0" # outDatumHash
        mayDatum <- pletC $ findDatum # odh
        pure $ pmatch mayDatum \case
          PNothing -> pcon PNothing
          PJust datum -> pcon $ PJust $ pfromData (ptryFromData (pto datum))

ptryFromData :: forall a s. PTryFrom PData (PAsData a) => Term s PData -> Term s (PAsData a)
ptryFromData x = unTermCont $ fst <$> tcont (ptryFrom @(PAsData a) x)

pscriptContextTxInfo :: Term s (PScriptContext :--> PTxInfo)
pscriptContextTxInfo = phoistAcyclic $ plam $ \ctx -> pfield @"txInfo" # ctx

pownCurrencySymbol :: Term s (PScriptContext :--> PCurrencySymbol)
pownCurrencySymbol = phoistAcyclic $
  plam $ \ctx -> unTermCont $ do
    ptraceC "pownCurrencySymbol"
    purpose <- pmatchC $ pfield @"purpose" # ctx
    pure $ case purpose of
      PMinting cs -> pfield @"_0" # cs
      _ -> ptraceError "Script purpose is not 'Minting'!"

ptxInfoMint :: Term s (PTxInfo :--> PValue 'PValue.Sorted 'PValue.NonZero)
ptxInfoMint = phoistAcyclic $
  plam $ \txInfo -> unTermCont $ do
    ptraceC "ptxInfoMint"
    PTxInfo ti <- pmatchC txInfo
    pure $ pnormalize #$ pfield @"mint" # ti

ptxInfoOutputs :: Term s (PTxInfo :--> PBuiltinList PTxOut)
ptxInfoOutputs = plam $ \info -> pfield @"outputs" # info

ptxOutValue :: Term s (PTxOut :--> PValue 'PValue.Sorted 'PValue.NonZero)
ptxOutValue = plam $ \out -> PValue.pnormalize #$ pfield @"value" # out

-- | Check if a transaction was signed by the given public key.
ptxSignedBy :: Term s (PTxInfo :--> PPubKeyHash :--> PBool)
ptxSignedBy = phoistAcyclic $
  plam $ \txInfo pkh -> unTermCont $ do
    ptraceC "txSignedBy"
    PTxInfo ti <- pmatchC txInfo
    sigs <- pletC $ pfield @"signatories" # ti
    maySig <- pletC $ pfind # plam (\sig -> pfromData sig #== pkh) #$ sigs
    pure $ pmatch maySig \case
      PNothing -> pcon PFalse
      _ -> pcon PTrue

-- | Find the data corresponding to a data hash, if there is one
pfindDatum :: Term s (PTxInfo :--> PDatumHash :--> PMaybe PDatum)
pfindDatum = phoistAcyclic $
  plam $ \txInfo dh -> unTermCont $ do
    ptraceC "findDatum"
    PTxInfo ti <- pmatchC txInfo
    ds <- pletC $ pfield @"datums" # ti
    mayPair <- pletC $ pfind # plam (\pair -> pfield @"_0" # pfromData pair #== dh) #$ pfromData ds
    pure $ pmatch mayPair \case
      PNothing -> pcon PNothing
      PJust pair -> pcon (PJust (pfield @"_1" # pfromData pair))
