{-# LANGUAGE BlockArguments #-}

module Coop.Plutus.Aux (
  phasCurrency,
  pboolC,
  pmaybeDataC,
  punit,
  ptryFromData,
  pownCurrencySymbol,
  pfindDatum,
  mkOneShotMintingPolicy,
  pfindMap,
  pdatumFromTxOut,
  pmustMint,
  pmustValidateAfter,
  pmustBeSignedBy,
  pcurrencyTokens,
  pdjust,
  pdnothing,
  pmustSpend,
  pmustPayTo,
  pfindOwnInput',
  pfoldTxOutputs,
  pfoldTxInputs,
  pmustHandleSpentWithMp,
  pcurrencyValue,
  pmustSpendAtLeast,
) where

import Control.Monad.Fail (MonadFail (fail))
import Plutarch (TermCont, popaque, pto)
import Plutarch.Api.V1.AssocMap (pempty, plookup, psingleton)
import Plutarch.Api.V1.Value (pnoAdaValue, pnormalize, pvalueOf)
import Plutarch.Api.V1.Value qualified as PValue
import Plutarch.Api.V2 (AmountGuarantees (NonZero), KeyGuarantees (Sorted), PAddress, PCurrencySymbol, PDatum, PDatumHash, PExtended, PInterval (PInterval), PLowerBound (PLowerBound), PMap (PMap), PMaybeData (PDJust, PDNothing), PMintingPolicy, POutputDatum (PNoOutputDatum, POutputDatum, POutputDatumHash), PPOSIXTime, PPubKeyHash, PScriptContext, PScriptPurpose (PMinting, PSpending), PTokenName, PTuple, PTxInInfo, PTxOut, PTxOutRef, PUpperBound, PValue (PValue))
import Plutarch.Bool (PBool (PTrue))
import Plutarch.DataRepr (pdcons)
import Plutarch.Extra.Interval (pcontains)
import Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC, ptraceC)
import Plutarch.List (PIsListLike, PListLike (pelimList), pany)
import Plutarch.Num (PNum ((#+)))
import Plutarch.Prelude (ClosedTerm, PAsData, PBool (PFalse), PBuiltinList, PData, PEq ((#==)), PInteger (), PIsData, PMaybe (PJust, PNothing), PPartialOrd ((#<=)), PTryFrom, PUnit, S, Term, getField, pcon, pconstant, pconstantData, pdata, pdnil, pelem, pfield, pfind, pfix, pfoldl, pfromData, pfstBuiltin, phoistAcyclic, pif, plam, plet, pmap, pmatch, ptraceError, ptryFrom, (#), (#$), type (:-->))
import Plutarch.TermCont (TermCont (runTermCont), tcont, unTermCont)
import PlutusLedgerApi.V1 (Extended (PosInf), UpperBound (UpperBound))
import Prelude (Applicative (pure), Bool (False, True), Monad ((>>)), Monoid (mempty), fst, ($), (<$>), (>>=))

{- | Check if a 'PValue' contains the given currency symbol.
NOTE: MangoIV says the plookup should be inlined here
-}
phasCurrency :: forall (q :: AmountGuarantees) (s :: S). Term s (PCurrencySymbol :--> PValue 'PValue.Sorted q :--> PBool)
phasCurrency = phoistAcyclic $
  plam $ \cs val ->
    pmatch
      (plookup # cs # pto val)
      ( \case
          PNothing -> pcon PFalse
          _ -> pcon PTrue
      )

pcurrencyTokens :: forall (q :: AmountGuarantees) (s :: S). Term s (PCurrencySymbol :--> PValue 'PValue.Sorted q :--> PMap 'Sorted PTokenName PInteger)
pcurrencyTokens = phoistAcyclic $
  plam $ \cs val ->
    pmatch
      (plookup # cs # pto val)
      ( \case
          PNothing -> pempty
          PJust tokens -> tokens
      )

pcurrencyValue :: forall (q :: AmountGuarantees) (s :: S). Term s (PCurrencySymbol :--> PValue 'Sorted q :--> PValue 'Sorted 'NonZero)
pcurrencyValue = phoistAcyclic $
  plam $ \cs val ->
    pmatch
      (plookup # cs # pto val)
      ( \case
          PNothing -> mempty @(Term _ (PValue 'Sorted 'NonZero))
          PJust tokens -> pnormalize # pcon (PValue $ psingleton # cs # tokens)
      )

pboolC :: TermCont @r s a -> TermCont @r s a -> Term s PBool -> TermCont @r s a
pboolC f t b = tcont $ \k -> pif b (runTermCont t k) (runTermCont f k)

pmaybeDataC :: PIsData a => TermCont @r s b -> (Term s a -> TermCont @r s b) -> Term s (PMaybeData a) -> TermCont @r s b
pmaybeDataC l r m = tcont $ \k -> pmatch m \case
  PDNothing _ -> runTermCont l k
  PDJust x -> runTermCont (r (pfield @"_0" # x)) k

punit :: Term s PUnit
punit = pconstant ()

ptryFromData :: forall a s. PTryFrom PData (PAsData a) => Term s PData -> Term s (PAsData a)
ptryFromData x = unTermCont $ fst <$> tcont (ptryFrom @(PAsData a) x)

pownCurrencySymbol :: Term s (PScriptPurpose :--> PCurrencySymbol)
pownCurrencySymbol = phoistAcyclic $
  plam $ \purpose -> unTermCont do
    ptraceC "pownCurrencySymbol"
    pure $ pmatch purpose \case
      PMinting cs -> pfield @"_0" # cs
      _ -> ptraceError "pownCurrencySymbol: Script purpose is not 'Minting'!"

pfindOwnInputV2 :: Term s (PBuiltinList PTxInInfo :--> PTxOutRef :--> PMaybe PTxInInfo)
pfindOwnInputV2 = phoistAcyclic $
  plam $ \inputs outRef ->
    pfind # (matches # outRef) # inputs
  where
    matches :: Term s (PTxOutRef :--> PTxInInfo :--> PBool)
    matches = phoistAcyclic $
      plam $ \outref txininfo ->
        outref #== pfield @"outRef" # txininfo

pfindOwnInput' :: Term s (PScriptContext :--> PTxInInfo)
pfindOwnInput' = phoistAcyclic $
  plam $ \ctx -> unTermCont do
    ptraceC "pfindOwnInput'"
    ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
    txInfo <- pletFieldsC @'["inputs"] ctx'.txInfo
    pmatchC ctx'.purpose >>= \case
      PSpending txOutRef ->
        pmatchC
          (pfindOwnInputV2 # txInfo.inputs # (pfield @"_0" # txOutRef))
          >>= \case
            PNothing -> fail "pfindOwnInput': Script purpose is not 'Spending'!"
            PJust txInInfo -> pure txInInfo
      _ -> fail "pfindOwnInput': Script purpose is not 'Spending'!"

-- | Find the data corresponding to a data hash, if there is one
pfindDatum :: Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum)) :--> PDatumHash :--> PMaybeData PDatum)
pfindDatum = phoistAcyclic $
  plam $ \datums dh -> unTermCont do
    ptraceC "pfindDatum"
    pure $
      pfindMap
        # plam
          ( \pair -> unTermCont do
              pair' <- pletFieldsC @'["_0", "_1"] $ pfromData pair
              dh' <- pletC $ getField @"_0" pair'
              datum <- pletC $ getField @"_1" pair'
              pure $
                pif
                  (dh' #== dh)
                  (pcon $ PDJust $ pdcons # pdata datum # pdnil)
                  (pcon $ PDNothing pdnil)
          )
        #$ datums

-- NOTE: MangoIV warns against (de)constructing Maybe values like this.
pfindMap :: PIsListLike l a => Term s ((a :--> PMaybeData b) :--> l a :--> PMaybeData b)
pfindMap = phoistAcyclic $
  plam \f -> pfix #$ plam $ \self xs ->
    pelimList
      ( \y ys ->
          plet
            (f # y)
            ( \may -> pmatch may \case
                PDNothing _ -> self # ys
                PDJust res -> pcon $ PDJust res
            )
      )
      (pcon $ PDNothing pdnil)
      xs

{- | Minting policy for OneShot tokens.

Ensures a given `TxOutRef` is consumed to enforce uniqueness of the token.
`q` tokens can be minted at a time.
-}
mkOneShotMintingPolicy ::
  ClosedTerm
    ( PAsData PInteger :--> PAsData PTokenName
        :--> PAsData PTxOutRef
        :--> PMintingPolicy
    )
mkOneShotMintingPolicy = phoistAcyclic $
  plam $ \q tn txOutRef _ ctx -> unTermCont do
    ptraceC "mkOneShotMintingPolicy"
    ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
    txInfo <- pletFieldsC @'["inputs", "mint"] ctx'.txInfo
    inputs <- pletC $ pfromData txInfo.inputs
    mint <- pletC $ pfromData $ txInfo.mint
    cs <- pletC $ pownCurrencySymbol # ctx'.purpose

    pboolC
      (fail "mkOneShotMintingPolicy: Doesn't consume utxo")
      (pure punit)
      (pconsumesRef # pfromData txOutRef # inputs)

    pboolC
      (fail "mkOneShotMintingPolicy: Incorrect minted value")
      (pure $ popaque punit)
      (pvalueOf # mint # cs # pfromData tn #== pfromData q)

-- | Check if utxo is consumed
pconsumesRef :: Term s (PTxOutRef :--> PBuiltinList PTxInInfo :--> PBool)
pconsumesRef = phoistAcyclic $
  plam $ \txOutRef ->
    pany #$ plam $ \input -> unTermCont do
      txOutRef' <- pletC $ pfield @"outRef" # input
      pure $ txOutRef #== txOutRef'

pdatumFromTxOut :: forall a (s :: S). (PIsData a, PTryFrom PData (PAsData a)) => Term s (PScriptContext :--> PTxOut :--> a)
pdatumFromTxOut = phoistAcyclic $
  plam $ \ctx txOut -> unTermCont do
    -- TODO: Migrate to inline datums
    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["datums"] ctx'.txInfo

    datum <-
      pmatchC (pfield @"datum" # txOut) >>= \case
        PNoOutputDatum _ -> fail "pDatumFromTxOut: Must have a datum present in the output"
        POutputDatumHash r -> do
          ptraceC "pDatumFromTxOut: Got a datum hash"
          pmatchC (plookup # pfromData (pfield @"datumHash" # r) # txInfo.datums) >>= \case
            PNothing -> (fail "pDatumFromTxOut: no datum with a given hash present in the transaction datums")
            PJust datum -> pure datum
        POutputDatum r -> do
          ptraceC "pDatumFromTxOut: Got a datum"
          pure (pfield @"outputDatum" # r)

    pure $ pfromData (ptryFromData @a (pto datum))

pmustMint :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PTokenName :--> PInteger :--> PUnit)
pmustMint = phoistAcyclic $
  plam $ \ctx cs tn q -> unTermCont do
    ptraceC "mustMint"
    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["mint"] ctx'.txInfo
    mint <- pletC $ txInfo.mint
    pboolC
      (fail "pmustMint: didn't mint the specified quantity")
      (ptraceC "pmustMint: minted specified quantity" >> pure punit)
      (pvalueOf # mint # cs # tn #== q)

pmustValidateAfter :: ClosedTerm (PScriptContext :--> PExtended PPOSIXTime :--> PUnit)
pmustValidateAfter = phoistAcyclic $
  plam $ \ctx after -> unTermCont do
    ptraceC "mustValidateAfter"
    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["validRange"] (getField @"txInfo" ctx')

    txValidRange <- pletC $ pfromData $ getField @"validRange" txInfo
    pboolC
      (fail "pmustValidateAfter: transaction validation range is not after 'after'")
      (ptraceC "pmustValidateAfter: transaction validation range is after 'after'" >> pure punit)
      (pcontains # (pinterval' # pdata (plowerBound # after) # pdata pposInf) # txValidRange)

-- | interval from upper and lower
pinterval' ::
  forall a (s :: S).
  PIsData a =>
  Term
    s
    ( PAsData (PLowerBound a)
        :--> PAsData (PUpperBound a)
        :--> PInterval a
    )
pinterval' = phoistAcyclic $
  plam $ \lower upper ->
    pcon $
      PInterval $
        pdcons @"from" # lower
          #$ pdcons @"to" # upper # pdnil

plowerBound :: Term s (PExtended a :--> PLowerBound a)
plowerBound = phoistAcyclic $ plam \start -> pcon $ PLowerBound $ pdcons @"_0" # pdata start #$ pdcons @"_1" # pconstantData False # pdnil

pposInf :: Term s (PUpperBound PPOSIXTime)
pposInf = pconstant $ UpperBound PosInf True

pmustBeSignedBy :: ClosedTerm (PScriptContext :--> PPubKeyHash :--> PUnit)
pmustBeSignedBy = phoistAcyclic $
  plam $ \ctx pkh -> unTermCont do
    ptraceC "mustBeSignedBy"
    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["signatories"] (getField @"txInfo" ctx')
    sigs <- pletC $ getField @"signatories" txInfo
    pboolC
      (fail "mustBeSignedBy: pkh didn't sign the transaction")
      (ptraceC "mustBeSignedBy: pkh signed the transaction" >> pure punit)
      (pelem # pdata pkh # sigs)

-- | Foldl over transaction outputs
pfoldTxOutputs :: ClosedTerm (PScriptContext :--> (a :--> PTxOut :--> a) :--> a :--> a)
pfoldTxOutputs = phoistAcyclic $
  plam $ \ctx foldFn initial -> unTermCont do
    ptraceC "pfoldTxInputs"
    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["outputs"] ctx'.txInfo

    pure $
      pfoldl
        # foldFn
        # initial
        # pfromData txInfo.outputs

-- | Checks total tokens spent
pmustPayTo :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PTokenName :--> PInteger :--> PAddress :--> PBool)
pmustPayTo = phoistAcyclic $
  plam $ \ctx cs tn mustPayQ addr -> unTermCont do
    ptraceC "pmustPayTo"
    paidQ <-
      pletC $
        pfoldTxOutputs # ctx
          # plam
            ( \paid txOut -> unTermCont do
                txOut' <- pletFieldsC @'["value", "address"] txOut

                pboolC
                  (pure paid)
                  (pure $ paid #+ (pvalueOf # txOut'.value # cs # tn))
                  (txOut'.address #== addr)
            )
          # 0

    pboolC
      (fail "pmustPayTo: Must pay the specified quantity")
      (ptraceC "pmustPayTo: Paid the specified quantity" >> pure (pcon PTrue))
      (mustPayQ #== paidQ)

-- | Foldl over transaction inputs
pfoldTxInputs :: ClosedTerm (PScriptContext :--> (a :--> PTxInInfo :--> a) :--> a :--> a)
pfoldTxInputs = phoistAcyclic $
  plam $ \ctx foldFn initial -> unTermCont do
    ptraceC "pfoldTxInputs"
    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["inputs"] ctx'.txInfo

    pure $
      pfoldl
        # foldFn
        # initial
        # pfromData txInfo.inputs

-- | Checks total tokens spent
pmustSpendPred :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PTokenName :--> (PInteger :--> PBool) :--> PBool)
pmustSpendPred = phoistAcyclic $
  plam $ \ctx cs tn predOnQ -> unTermCont do
    ptraceC "pmustSpendPred"
    spentQ <-
      pletC $
        pfoldTxInputs # ctx
          # plam
            ( \spent txInInfo -> unTermCont do
                resolved <- pletFieldsC @'["value"] $ pfield @"resolved" # txInInfo

                pure $ spent #+ (pvalueOf # resolved.value # cs # tn)
            )
          # 0

    pboolC
      (fail "pmustSpendPred: didn't spend the required quantity")
      (ptraceC "pmustSpendPred: spent required quantity" >> pure (pcon PTrue))
      (predOnQ # spentQ)

-- | Checks total tokens spent
pmustSpend :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PTokenName :--> PInteger :--> PBool)
pmustSpend = phoistAcyclic $
  plam $ \ctx cs tn mustSpendQ -> pmustSpendPred # ctx # cs # tn # plam (#== mustSpendQ)

-- | Checks total tokens spent
pmustSpendAtLeast :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PTokenName :--> PInteger :--> PBool)
pmustSpendAtLeast = phoistAcyclic $
  plam $ \ctx cs tn mustSpendAtLeastQ -> pmustSpendPred # ctx # cs # tn # plam (mustSpendAtLeastQ #<=)

pmustHandleSpentWithMp :: ClosedTerm (PScriptContext :--> PBool)
pmustHandleSpentWithMp = phoistAcyclic $
  plam $ \ctx -> unTermCont do
    ptraceC "pmustHandleSpentWithMp"

    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["mint"] ctx'.txInfo
    mint <- pletC $ pto $ pfromData txInfo.mint

    ownIn <- pletC $ pfindOwnInput' # ctx
    ownInVal <- pletC $ pnoAdaValue #$ pfield @"value" # (pfield @"resolved" # ownIn)
    pmatchC (pto ownInVal) >>= \case
      PMap elems ->
        pletC $
          pmap
            # plam
              ( \kv -> unTermCont do
                  cs <- pletC $ pfromData $ pfstBuiltin # kv
                  pmatchC (plookup # cs # mint) >>= \case
                    PNothing -> fail "pmustHandleSpentWithMp: Spent currency symbol must be in mint"
                    PJust _ -> pure punit
              )
            # elems
    ptraceC "pmustHandleSpentWithMp: All spent currency symbols are in mint"

    pure $ pcon PTrue

pdnothing :: Term s (PMaybeData a)
pdnothing = pcon $ PDNothing pdnil

pdjust :: PIsData a => Term s a -> Term s (PMaybeData a)
pdjust x = pcon $ PDJust $ pdcons # pdata x # pdnil
