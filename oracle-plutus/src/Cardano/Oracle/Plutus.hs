{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cardano.Oracle.Plutus (
  ptxOutValue,
  ptxSignedBy,
  resourceMintingPolicy,
  resourceValidator,
  mkOneShotMintingPolicy,
  ptxInfoMint,
  ResourceMintingParams (..),
) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic)
import Plutarch (Config, DerivePlutusType (DPTStrat), POpaque, TermCont, popaque, pto)
import Plutarch.Api.V1 (
  PAddress,
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
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Api.V1.Value (AmountGuarantees, KeyGuarantees, PTokenName (PTokenName), pnormalize, pvalueOf)
import Plutarch.Api.V1.Value qualified as PValue
import Plutarch.Bool (PBool (PTrue))
import Plutarch.ByteString (PByteString, plengthBS)
import Plutarch.DataRepr (
  PDataFields,
  PlutusTypeData,
 )
import Plutarch.Extra.TermCont (pletC, pmatchC, ptraceC)
import Plutarch.List (PIsListLike, PListLike (pelimList), pany)
import Plutarch.Prelude (ClosedTerm, PAsData, PBool (PFalse), PBuiltinList, PData, PDataRecord, PEq ((#==)), PIsData, PLabeledType ((:=)), PMaybe (PJust, PNothing), PString, PTryFrom, PUnit (PUnit), PlutusType, S, Term, pcon, pconstant, pdata, pelem, pfield, pfind, pfix, pfromData, phoistAcyclic, pif, plam, plet, pmap, pmatch, ptrace, ptraceError, ptryFrom, (#), (#$), (#&&), type (:-->))
import Plutarch.TermCont (TermCont (runTermCont), tcont, unTermCont)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1 (Address, CurrencySymbol, LedgerBytes)
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Scripts (MintingPolicy, Validator)
import Prelude (Applicative (pure), Eq, Show, fst, snd, ($), (.), (<$>))

type ResourceDescription = LedgerBytes
type Resource = LedgerBytes

data ResourceDatum = ResourceDatum
  { submittedBy :: PubKeyHash
  , publishedBy :: PubKeyHash
  , description :: ResourceDescription
  , resource :: Resource
  }
  deriving stock (Show, GHC.Generic, Eq)

data PResourceDatum s
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
instance PTryFrom PData (PAsData PResourceDatum) -- FIXME: This probably doesn't do anything

-- FIXME: Integrate https://github.com/Plutonomicon/plutarch-plutus/pull/520
instance PTryFrom PData (PAsData PPubKeyHash) where
  type PTryFromExcess PData (PAsData PPubKeyHash) = Flip Term PPubKeyHash
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    tcont $ \f -> pif (plengthBS # unwrapped #== 28) (f ()) (ptraceError "a PubKeyHash must be 28 bytes long")
    pure (punsafeCoerce opq, pcon . PPubKeyHash $ unwrapped)

newtype Flip f a b = Flip (f b a) deriving stock (GHC.Generic)

data ResourceMintingParams = ResourceMintingParams
  { rmp'instanceCs :: CurrencySymbol -- provided by the one shot mp,
  , rmp'resourceValidatorAddress :: Address
  }

-- TODO: Parametrize by the one-shot token and resourceMintingPolicy
resourceValidator :: Config -> Validator
resourceValidator cfg = mkValidator cfg $ plam $ \_ _ _ -> popaque $ pconstant ()

-- | Minting policy that validates creation of new resources.
resourceMintingPolicy :: Config -> ResourceMintingParams -> MintingPolicy
resourceMintingPolicy cfg params = mkMintingPolicy cfg $
  plam $ \_ ctx -> unTermCont do
    _ <- pletC $ parseOutputs # pconstant (rmp'resourceValidatorAddress params) # ctx
    _ <- pletC $ pconstant (rmp'instanceCs params) -- TODO: Ask if this gets purged during compilation!
    pure $ popaque $ pconstant ()

{- | Parse and validate transaction outputs that hold resources.
 We rely on the level 1 ledger rule to verify that what was minted, was sent out.
 So for each of the resource output we check:
  - does it hold own currency symbol?
    - no -> skip processing of the current output (we don't err and so enable richer transactions)
    - yes
      - TODO: does it send the output to resourceValidator? yes -> continue; no -> err
      - is the transaction signed by the publisher specified in the ResourceDatum? yes -> continue; no -> err
      - is there a single token of own currency symbol and publisher as the token name? yes -> continue; no -> err
 We don't enforce rewards at the minting policy, this is left open for the publisher to include and the submitter to accept.
 TODO: Switch to using Plutus V2
-}
parseOutputs :: Term s (PAddress :--> PScriptContext :--> PBuiltinList PBool)
parseOutputs = phoistAcyclic $
  plam $ \resValAddr ctx -> unTermCont $ do
    ptraceC "parseOutputs"
    ownCs <- pletC $ pownCurrencySymbol # ctx
    txInfo <- pletC $ pscriptContextTxInfo # ctx
    findDatum' <- pletC $ pfindDatum # txInfo
    sigs <- pletC $ ptxInfoSignatories # txInfo
    txOuts <- pletC $ ptxInfoOutputs # txInfo
    parseOutputWithResource' <- pletC $ parseOutputWithResource # resValAddr # ownCs # sigs # findDatum'
    pure $ pmap # parseOutputWithResource' # txOuts

parseOutputWithResource :: Term s (PAddress :--> PCurrencySymbol :--> PBuiltinList (PAsData PPubKeyHash) :--> (PDatumHash :--> PMaybe PDatum) :--> PTxOut :--> PBool)
parseOutputWithResource = phoistAcyclic $
  plam $ \resValAddr ownCs sigs findDatum txOut -> unTermCont do
    ptraceC "parseOutputWithResource"
    outVal <- pletC $ pnormalize #$ pfield @"value" # txOut
    outAddr <- pletC $ pfield @"address" # txOut
    hasOwnCs <- pletC $ phasCurrency # ownCs # outVal
    pure $ pmatch hasOwnCs \case
      PFalse -> unTermCont do
        ptraceC "parseOutputWithResource: own token not in the output, skipping"
        pure $ pcon PTrue
      PTrue -> unTermCont do
        ptraceC "parseOutputWithResource: found own token in the output, continuing"
        mayOutDatumHash <- pletC $ pfield @"datumHash" # txOut
        pure $ pmatch mayOutDatumHash \case
          PDNothing _ -> ptraceError "parseOutputWithResource: no datum present in the output"
          PDJust outDatumHash -> unTermCont $ do
            odh <- pletC $ pfield @"_0" # outDatumHash
            mayDatum <- pletC $ findDatum # odh
            pure $ pmatch mayDatum \case
              PNothing -> ptraceError "parseOutputWithResource: no datum with a given hash present in the transaction datums"
              PJust datum -> unTermCont $ do
                resDat <- pletC $ pfromData (ptryFromData @PResourceDatum (pto datum))
                publishedBy <- pletC $ pfield @"publishedBy" # resDat
                PPubKeyHash publishedByBytes <- pmatchC publishedBy
                publishedByTokenName <- pletC $ pcon $ PTokenName publishedByBytes
                quantity <- pletC $ pvalueOf # outVal # ownCs # publishedByTokenName
                hasSinglePublisherToken <- pletC $ quantity #== 1
                ptraceBoolC
                  "parseOutputWithResource: invalid resource minting asset"
                  "parseOutputWithResource: valid resource minting asset"
                  hasSinglePublisherToken

                publisherIsSignatory <- pletC $ pelem # (pfield @"publishedBy" # resDat) # sigs
                ptraceBoolC
                  "parseOutputWithResource: publisher didn't sign the transaction"
                  "parseOutputWithResource: publisher signed the transaction"
                  publisherIsSignatory
                sentToResourceValidator <- pletC $ outAddr #== resValAddr
                ptraceBoolC
                  "parseOutputWithResource: output not sent to resourceValidator"
                  "parseOutputWithResource: output sent to resourceValidator"
                  sentToResourceValidator
                pure $
                  pif
                    (hasSinglePublisherToken #&& publisherIsSignatory #&& sentToResourceValidator)
                    (ptraceError "parseOutputWithResource: failed")
                    (pcon PTrue)

-- | Check if a 'PValue' contains the given currency symbol.
phasCurrency :: Term s (PCurrencySymbol :--> PValue 'PValue.Sorted 'PValue.NonZero :--> PBool)
phasCurrency = phoistAcyclic $
  plam $ \cs val ->
    pmatch
      (plookup # cs # pto val)
      ( \case
          PNothing -> pcon PFalse
          _ -> pcon PTrue
      )

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
  plam $ \ctx -> unTermCont do
    ptraceC "pownCurrencySymbol"
    purpose <- pmatchC $ pfield @"purpose" # ctx
    pure $ case purpose of
      PMinting cs -> pfield @"_0" # cs
      _ -> ptraceError "Script purpose is not 'Minting'!"

ptxInfoMint :: Term s (PTxInfo :--> PValue 'PValue.Sorted 'PValue.NonZero)
ptxInfoMint = phoistAcyclic $
  plam $ \txInfo -> unTermCont do
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
  plam $ \txInfo pkh -> unTermCont do
    ptraceC "txSignedBy"
    sigs <- pletC $ pfield @"signatories" # txInfo
    maySig <- pletC $ pfind # plam (\sig -> pfromData sig #== pkh) #$ sigs
    pure $ pmatch maySig \case
      PNothing -> pcon PFalse
      _ -> pcon PTrue

-- | Find the data corresponding to a data hash, if there is one
pfindDatum :: Term s (PTxInfo :--> PDatumHash :--> PMaybe PDatum)
pfindDatum = phoistAcyclic $
  plam $ \txInfo dh -> unTermCont do
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
