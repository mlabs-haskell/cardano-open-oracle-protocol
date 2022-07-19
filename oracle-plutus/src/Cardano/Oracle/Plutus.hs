{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cardano.Oracle.Plutus (ptxOutValue, ptxSignedBy, resourceMintingPolicy, resourceUtxoValidator, mkOneShotMintingPolicy) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic)
import Plutarch (Config, DerivePlutusType (DPTStrat), POpaque, TermCont, popaque, pto)
import Plutarch.Api.V1 (
  PCurrencySymbol,
  PDatum,
  PDatumHash,
  PMaybeData (PDJust, PDNothing),
  PPubKeyHash (PPubKeyHash),
  PScriptContext,
  PScriptPurpose (PMinting),
  PTxInInfo,
  PTxInfo,
  PTxOut,
  PTxOutRef,
  PValue,
  mkMintingPolicy,
  mkValidator,
 )
import Plutarch.Api.V1.Value (AmountGuarantees, KeyGuarantees, PTokenName (PTokenName), pnormalize, pvalueOf)
import Plutarch.Api.V1.Value qualified as PValue
import Plutarch.Bool (PBool (PTrue))
import Plutarch.DataRepr (
  PlutusTypeData,
 )
import Plutarch.Extra.TermCont (pletC, pmatchC, ptraceC)
import Plutarch.List (PIsListLike, PListLike (pelimList), pany)
import Plutarch.Prelude (ClosedTerm, PAsData, PBool (PFalse), PBuiltinList, PData, PDataRecord, PEq ((#==)), PIsData, PLabeledType ((:=)), PMaybe (PJust, PNothing), PString, PTryFrom, PUnit (PUnit), PlutusType, S, Term, pcon, pconstant, pdata, pelem, pfield, pfind, pfix, pfromData, phoistAcyclic, pif, plam, plet, pmatch, ptrace, ptraceError, ptryFrom, (#), (#$), (#&&), type (:-->))
import Plutarch.TermCont (tcont, unTermCont)
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Scripts (MintingPolicy, Validator)
import Prelude (Applicative (pure), String, fst, ($), (<$>))
import Prelude qualified as Hask

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

resourceUtxoValidator :: Config -> Validator
resourceUtxoValidator cfg = mkValidator cfg $ plam $ \_ _ _ -> popaque $ pconstant ()

resourceMintingPolicy :: Config -> MintingPolicy
resourceMintingPolicy cfg = mkMintingPolicy cfg $
  plam $ \_ ctx -> unTermCont do
    info <- pletC $ pscriptContextTxInfo # ctx
    _ownCs <- pletC $ pownCurrencySymbol # ctx
    _minted <- pletC $ ptxInfoMint # info
    -- resourceAssetMinted <- pletC $ pvalueOf # minted # ownCs # (PValue.PTokenName # "asd")
    _ <- pletC $ findOutputWithResource # ctx
    pure $ popaque $ pconstant ()

findOutputWithResource :: Term s (PScriptContext :--> PMaybe PResourceDatum)
findOutputWithResource = phoistAcyclic $
  plam $ \ctx -> unTermCont $ do
    ptraceC "findOutputWithResource"
    ownCs <- pletC $ pownCurrencySymbol # ctx
    txInfo <- pletC $ pscriptContextTxInfo # ctx
    findDatum' <- pletC $ pfindDatum # txInfo
    sigs <- pletC $ ptxInfoSignatories # txInfo
    txOuts <- pletC $ ptxInfoOutputs # txInfo
    pure $
      pfindMap
        # plam
          (\out -> parseOutputWithResource # ownCs # sigs # findDatum' # out)
        #$ txOuts

parseOutputWithResource :: Term s (PCurrencySymbol :--> PBuiltinList (PAsData PPubKeyHash) :--> (PDatumHash :--> PMaybe PDatum) :--> PTxOut :--> PMaybe PResourceDatum)
parseOutputWithResource = phoistAcyclic $
  plam $ \ownCs sigs findDatum txOut -> unTermCont do
    ptraceC "parseOutputWithResource"
    outVal <- pletC $ pnormalize #$ pfield @"value" # txOut
    _outAddr <- pletC $ pfield @"address" # txOut
    mayOutDatumHash <- pletC $ pfield @"datumHash" # txOut
    pure $ pmatch mayOutDatumHash \case
      PDNothing _ -> unTermCont $ do
        ptraceC "parseOutputWithResource: no datum present in the output"
        pure $ pcon PNothing
      PDJust outDatumHash -> unTermCont $ do
        odh <- pletC $ pfield @"_0" # outDatumHash
        mayDatum <- pletC $ findDatum # odh
        pure $ pmatch mayDatum \case
          PNothing -> unTermCont $ do
            ptraceC "parseOutputWithResource: no datum with a given hash present in the transaction datums"
            pure $ pcon PNothing
          PJust datum -> unTermCont $ do
            mayResDat <- pletC $ pcon $ PJust $ pfromData (ptryFromData (pto datum))
            pure $ pmatch mayResDat \case
              PNothing -> unTermCont $ do
                ptraceC "parseOutputWithResource: could not coerce datum to ResourceDatum"
                pure $ pcon PNothing
              PJust resDat -> unTermCont do
                PResourceDatum rd <- pmatchC resDat
                publishedBy <- pletC $ pfield @"publishedBy" # rd
                PPubKeyHash publishedByBytes <- pmatchC publishedBy
                publishedByTokenName <- pletC $ pcon $ PTokenName publishedByBytes
                quantity <- pletC $ pvalueOf # outVal # ownCs # publishedByTokenName
                expectedQuantity <- pletC $ quantity #== 1
                ptraceBoolC
                  "parseOutputWithResource: invalid resource minting asset"
                  "parseOutputWithResource: valid resource minting asset"
                  expectedQuantity

                publisherIsSignatory <- pletC $ pelem # (pfield @"publishedBy" # rd) # sigs
                ptraceBoolC
                  "parseOutputWithResource: publisher didn't sign the transaction"
                  "parseOutputWithResource: publisher signed the transaction"
                  publisherIsSignatory
                pure $ pif (expectedQuantity #&& publisherIsSignatory) (pcon PNothing) (pcon $ PJust resDat)

ptraceBoolC :: Term s PString -> Term s PString -> Term s PBool -> TermCont s (Term s PUnit)
ptraceBoolC msgFalse msgTrue b =
  pure $
    pif
      b
      (ptrace msgFalse punit)
      (ptrace msgTrue punit)

punit :: Term s PUnit
punit = pcon PUnit

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
    pure $ pnormalize #$ pfield @"mint" # txInfo

ptxInfoOutputs :: Term s (PTxInfo :--> PBuiltinList PTxOut)
ptxInfoOutputs = plam $ \info -> pfield @"outputs" # info

ptxInfoSignatories :: Term s (PTxInfo :--> PBuiltinList (PAsData PPubKeyHash))
ptxInfoSignatories = plam $ \info -> pfield @"signatories" # info

ptxOutValue :: Term s (PTxOut :--> PValue 'PValue.Sorted 'PValue.NonZero)
ptxOutValue = plam $ \out -> PValue.pnormalize #$ pfield @"value" # out

-- | Check if a transaction was signed by the given public key.
ptxSignedBy :: Term s (PTxInfo :--> PPubKeyHash :--> PBool)
ptxSignedBy = phoistAcyclic $
  plam $ \txInfo pkh -> unTermCont $ do
    ptraceC "txSignedBy"
    sigs <- pletC $ pfield @"signatories" # txInfo
    maySig <- pletC $ pfind # plam (\sig -> pfromData sig #== pkh) #$ sigs
    pure $ pmatch maySig \case
      PNothing -> pcon PFalse
      _ -> pcon PTrue

-- | Find the data corresponding to a data hash, if there is one
pfindDatum :: Term s (PTxInfo :--> PDatumHash :--> PMaybe PDatum)
pfindDatum = phoistAcyclic $
  plam $ \txInfo dh -> unTermCont $ do
    ptraceC "findDatum"
    ds <- pletC $ pfield @"datums" # txInfo
    pure $
      pfindMap
        # plam
          ( \pair ->
              plet
                (pfield @"_0" # pfromData pair)
                \dh' ->
                  pif
                    (dh' #== dh)
                    (pcon $ PJust $ pfield @"_1" # pfromData pair)
                    (pcon PNothing)
          )
        #$ pfromData ds

pfindMap :: PIsListLike l a => Term s ((a :--> PMaybe b) :--> l a :--> PMaybe b)
pfindMap = phoistAcyclic $
  pfix #$ plam $ \self f xs ->
    pelimList
      ( \y ys ->
          plet
            (f # y)
            ( \may -> pmatch may \case
                PNothing -> self # f # ys
                PJust res -> pcon $ PJust res
            )
      )
      (pcon PNothing)
      xs

{-
    Minting policy for OneShot tokens.
    Ensures a given `TxOutRef` is consumed to enforce uniqueness of the token.
    Only a single token can be minted at a time.
-}
mkOneShotMintingPolicy ::
  ClosedTerm
    ( PTokenName
        :--> PTxOutRef
        :--> POpaque
        :--> PScriptContext
        :--> POpaque
    )
mkOneShotMintingPolicy = phoistAcyclic $
  plam $ \tn txOutRef _ ctx -> unTermCont do
    txInfo <- pletC $ pfield @"txInfo" # ctx
    inputs <- pletC $ pfromData $ pfield @"inputs" # pfromData txInfo
    mint <- pletC $ pfromData $ pfield @"mint" # txInfo

    cs <- pletC $ pownCurrencySymbol # ctx

    _ <-
      pure $
        pif
          (pconsumesRef # txOutRef # inputs)
          (ptraceError "mkOneShotMintingPolicy: Doesn't consume utxo")
          punit

    _ <-
      pure $
        pif
          (phasSingleToken # cs # tn # mint)
          (ptraceError "mkOneShotMintingPolicy: Incorrect minted value")
          punit

    pure $ popaque $ pconstant ()

-- | Check if utxo is consumed
pconsumesRef :: Term s (PTxOutRef :--> PBuiltinList PTxInInfo :--> PBool)
pconsumesRef = phoistAcyclic $
  plam $ \txOutRef ->
    pany #$ plam $ \input -> unTermCont do
      txOutRef' <- pletC $ pfield @"outRef" # input
      pure $ pdata txOutRef #== pdata txOutRef'

-- | Check if a value has exactly one of the given token
phasSingleToken ::
  forall {s :: S} {w1 :: KeyGuarantees} {w2 :: AmountGuarantees}.
  Term s (PCurrencySymbol :--> PTokenName :--> PValue w1 w2 :--> PBool)
phasSingleToken = phoistAcyclic $
  plam $ \cs tn v ->
    1 #== pvalueOf # v # cs # tn
