{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cardano.Oracle.Plutus (
  resourceMintingPolicy,
  resourceValidator,
  mkOneShotMintingPolicy,
  ResourceMintingParams (..),
) where

import Control.Monad.Fail (MonadFail (fail))
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
  pdcons,
 )
import Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC, ptraceC)
import Plutarch.List (PIsListLike, PListLike (pelimList), pany)
import Plutarch.Prelude (ClosedTerm, PAsData, PBool (PFalse), PBuiltinList, PData, PDataRecord, PEq ((#==)), PIsData, PLabeledType ((:=)), PMaybe (PNothing), PTryFrom, PUnit (PUnit), PlutusType, S, Term, getField, pcon, pconstant, pdata, pdnil, pelem, pfield, pfix, pfromData, phoistAcyclic, pif, plam, plet, pmap, pmatch, ptraceError, ptryFrom, (#), (#$), (#&&), type (:-->))
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
    txInfo <- pletC $ pfield @"txInfo" # ctx
    findDatum' <- pletC $ pfindDatum # txInfo
    txInfo' <- pletFieldsC @'["signatories", "outputs"] txInfo
    sigs <- pletC $ getField @"signatories" txInfo'
    txOuts <- pletC $ getField @"outputs" txInfo'
    parseOutput' <- pletC $ parseOutput # resValAddr # ownCs # sigs # findDatum'
    pure $ pmap # parseOutput' # txOuts

parseOutput :: Term s (PAddress :--> PCurrencySymbol :--> PBuiltinList (PAsData PPubKeyHash) :--> (PDatumHash :--> PMaybeData PDatum) :--> PTxOut :--> PBool)
parseOutput = phoistAcyclic $
  plam $ \resValAddr ownCs sigs findDatum txOut -> unTermCont do
    ptraceC "parseOutput"
    txOut' <- pletFieldsC @'["value"] txOut
    outVal <- pletC $ pnormalize #$ getField @"value" txOut'

    hasOwnCs <- pletC $ phasCurrency # ownCs # outVal
    pboolC
      ( do
          ptraceC "parseOutput: own token not in the output, skipping"
          pure $ pcon PTrue
      )
      ( do
          ptraceC "parseOutput: found own token in the output, continuing"
          pure $ parseOutputWithResource # resValAddr # ownCs # sigs # findDatum # txOut
      )
      hasOwnCs

parseOutputWithResource :: Term s (PAddress :--> PCurrencySymbol :--> PBuiltinList (PAsData PPubKeyHash) :--> (PDatumHash :--> PMaybeData PDatum) :--> PTxOut :--> PBool)
parseOutputWithResource = phoistAcyclic $
  plam $ \resValAddr ownCs sigs findDatum txOut -> unTermCont do
    ptraceC "parseOutputWithResource"
    txOut' <- pletFieldsC @'["value", "address", "datumHash"] txOut
    outVal <- pletC $ pnormalize #$ getField @"value" txOut'
    outAddr <- pletC $ getField @"address" txOut'

    outDatumHash <-
      pmaybeDataC
        (fail "parseOutputWithResource: no datum present in the output")
        pure
        (getField @"datumHash" txOut')
    datum <-
      pmaybeDataC
        (fail "parseOutputWithResource: no datum with a given hash present in the transaction datums")
        pure
        (findDatum # outDatumHash)

    resDat <- pletC $ pfromData (ptryFromData @PResourceDatum (pto datum))
    publishedBy <- pletC $ pfield @"publishedBy" # resDat
    PPubKeyHash publishedByBytes <- pmatchC publishedBy
    publishedByTokenName <- pletC $ pcon $ PTokenName publishedByBytes
    quantity <- pletC $ pvalueOf # outVal # ownCs # publishedByTokenName

    hasSinglePublisherToken <-
      pboolC
        (fail "parseOutputWithResource: invalid resource minting asset")
        ( do
            ptraceC "parseOutputWithResource: valid resource minting asset"
            pure $ pcon PTrue
        )
        (quantity #== 1)

    publisherIsSignatory <-
      pboolC
        (fail "parseOutputWithResource: publisher didn't sign the transaction")
        ( do
            ptraceC "parseOutputWithResource: publisher signed the transaction"
            pure $ pcon PTrue
        )
        (pelem # pdata publishedBy # sigs)

    sentToResourceValidator <-
      pboolC
        (fail "parseOutputWithResource: output not sent to resourceValidator")
        ( do
            ptraceC "parseOutputWithResource: output sent to resourceValidator"
            pure $ pcon PTrue
        )
        (outAddr #== resValAddr)

    pboolC
      (fail "parseOutputWithResource: failed")
      (pure $ pcon PTrue)
      (hasSinglePublisherToken #&& publisherIsSignatory #&& sentToResourceValidator)

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

pboolC :: TermCont @r s a -> TermCont @r s a -> Term s PBool -> TermCont @r s a
pboolC f t b = tcont $ \k -> pif b (runTermCont t k) (runTermCont f k)

pmaybeDataC :: PIsData a => TermCont @r s b -> (Term s a -> TermCont @r s b) -> Term s (PMaybeData a) -> TermCont @r s b
pmaybeDataC l r m = tcont $ \k -> pmatch m \case
  PDNothing _ -> runTermCont l k
  PDJust x -> runTermCont (r (pfield @"_0" # x)) k

punit :: Term s PUnit
punit = pcon PUnit

ptryFromData :: forall a s. PTryFrom PData (PAsData a) => Term s PData -> Term s (PAsData a)
ptryFromData x = unTermCont $ fst <$> tcont (ptryFrom @(PAsData a) x)

pownCurrencySymbol :: Term s (PScriptContext :--> PCurrencySymbol)
pownCurrencySymbol = phoistAcyclic $
  plam $ \ctx -> unTermCont do
    ptraceC "pownCurrencySymbol"
    purpose <- pmatchC $ pfield @"purpose" # ctx
    pure $ case purpose of
      PMinting cs -> pfield @"_0" # cs
      _ -> ptraceError "Script purpose is not 'Minting'!"

-- | Find the data corresponding to a data hash, if there is one
pfindDatum :: Term s (PTxInfo :--> PDatumHash :--> PMaybeData PDatum)
pfindDatum = phoistAcyclic $
  plam $ \txInfo dh -> unTermCont do
    ptraceC "findDatum"
    ds <- pletC $ pfield @"datums" # txInfo
    pure $
      pfindMap
        # plam
          ( \pair -> unTermCont do
              pair' <- pletC $ pfromData pair
              dh' <- pletC $ pfield @"_0" # pair'
              datum <- pletC $ pfield @"_1" # pair'
              pure $
                pif
                  (dh' #== dh)
                  (pcon $ PDJust $ pdcons # pdata datum # pdnil)
                  (pcon $ PDNothing pdnil)
          )
        #$ pfromData ds

pfindMap :: PIsListLike l a => Term s ((a :--> PMaybeData b) :--> l a :--> PMaybeData b)
pfindMap = phoistAcyclic $
  pfix #$ plam $ \self f xs ->
    pelimList
      ( \y ys ->
          plet
            (f # y)
            ( \may -> pmatch may \case
                PDNothing _ -> self # f # ys
                PDJust res -> pcon $ PDJust res
            )
      )
      (pcon $ PDNothing pdnil)
      xs

{-
    Minting policy for OneShot tokens.
    Ensures a given `TxOutRef` is consumed to enforce uniqueness of the token.
    Only a single token can be minted at a time.
-}
mkOneShotMintingPolicy ::
  ClosedTerm
    ( PTokenName
        :--> PAsData PTxOutRef
        :--> POpaque
        :--> PScriptContext
        :--> POpaque
    )
mkOneShotMintingPolicy = phoistAcyclic $
  plam $ \tn txOutRef _ ctx -> unTermCont do
    txInfo <- pletFieldsC @'["inputs", "mint"] (pfield @"txInfo" # ctx)
    inputs <- pletC $ pfromData $ getField @"inputs" txInfo
    mint <- pletC $ pfromData $ getField @"mint" txInfo
    cs <- pletC $ pownCurrencySymbol # ctx

    pboolC
      (fail "mkOneShotMintingPolicy: Doesn't consume utxo")
      (pure punit)
      (pconsumesRef # txOutRef # inputs)

    pboolC
      (fail "mkOneShotMintingPolicy: Incorrect minted value")
      (pure punit)
      (phasSingleToken # cs # tn # mint)

    pure $ popaque $ pconstant ()

-- | Check if utxo is consumed
pconsumesRef :: Term s (PAsData PTxOutRef :--> PBuiltinList PTxInInfo :--> PBool)
pconsumesRef = phoistAcyclic $
  plam $ \txOutRef ->
    pany #$ plam $ \input -> unTermCont do
      txOutRef' <- pletC $ pfield @"outRef" # input
      pure $ txOutRef #== txOutRef'

-- | Check if a value has exactly one of the given token
phasSingleToken ::
  forall (w1 :: KeyGuarantees) (w2 :: AmountGuarantees) (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PValue w1 w2 :--> PBool)
phasSingleToken = phoistAcyclic $
  plam $ \cs tn v ->
    1 #== pvalueOf # v # cs # tn
