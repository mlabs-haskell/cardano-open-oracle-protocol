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
  pmustSpendFromAddress,
  pmustMintCurrency,
  pvalueOfCurrency,
) where

import Control.Monad.Fail (MonadFail (fail))
import Plutarch (TermCont, popaque, pto)
import Plutarch.Api.V1 (AmountGuarantees (Positive), KeyGuarantees (Sorted), PAddress, PCurrencySymbol, PDatum, PDatumHash, PMap, PMaybeData (PDJust, PDNothing), PMintingPolicy, PPOSIXTime, PPubKeyHash, PScriptContext, PScriptPurpose (PMinting), PTokenName, PTuple, PTxInInfo, PTxOut, PTxOutRef, PValue)
import Plutarch.Api.V1.AssocMap (pempty, plookup)
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V1.Value qualified as PValue
import Plutarch.Bool (PBool (PTrue))
import Plutarch.DataRepr (pdcons)
import Plutarch.Extra.Interval (pbefore)
import Plutarch.Extra.TermCont (pletC, pletFieldsC, ptraceC)
import Plutarch.List (PIsListLike, PListLike (pelimList, pnil), pany, pfoldr)
import Plutarch.Num (PNum ((#+)))
import Plutarch.Prelude (ClosedTerm, PAsData, PBool (PFalse), PBuiltinList, PData, PEq ((#==)), PInteger (), PIsData, PMaybe (PJust, PNothing), PTryFrom, PUnit, S, Term, getField, pcon, pconstant, pdata, pdnil, pelem, pfield, pfix, pfoldl, pfromData, phoistAcyclic, pif, plam, plet, pmap, pmatch, psndBuiltin, ptraceError, ptryFrom, (#), (#$), type (:-->))
import Plutarch.TermCont (TermCont (runTermCont), tcont, unTermCont)
import Prelude (Applicative (pure), Monad ((>>)), fst, ($), (<$>))

{- | Check if a 'PValue' contains the given currency symbol.
NOTE: MangoIV says the plookup should be inlined here
TODO: Implement in terms of pcurrencyTokens
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
Only a single token can be minted at a time.
-}
mkOneShotMintingPolicy ::
  ClosedTerm
    ( PAsData PTokenName
        :--> PAsData PTxOutRef
        :--> PMintingPolicy
    )
mkOneShotMintingPolicy = phoistAcyclic $
  plam $ \tn txOutRef _ ctx -> unTermCont do
    ptraceC "mkOneShotMintingPolicy"
    ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
    txInfo <- pletFieldsC @'["inputs", "mint"] (getField @"txInfo" ctx')
    inputs <- pletC $ pfromData $ getField @"inputs" txInfo
    mint <- pletC $ pfromData $ getField @"mint" txInfo
    cs <- pletC $ pownCurrencySymbol # getField @"purpose" ctx'

    pboolC
      (fail "mkOneShotMintingPolicy: Doesn't consume utxo")
      (pure punit)
      (pconsumesRef # pfromData txOutRef # inputs)

    pboolC
      (fail "mkOneShotMintingPolicy: Incorrect minted value")
      (pure $ popaque punit)
      (phasSingleToken # cs # pfromData tn # mint)

-- | Check if utxo is consumed
pconsumesRef :: Term s (PTxOutRef :--> PBuiltinList PTxInInfo :--> PBool)
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

pdatumFromTxOut :: forall a (s :: S). (PIsData a, PTryFrom PData (PAsData a)) => Term s (PScriptContext :--> PTxOut :--> a)
pdatumFromTxOut = phoistAcyclic $
  plam $ \ctx txOut -> unTermCont do
    -- TODO: Migrate to inline datums
    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["datums"] (getField @"txInfo" ctx')

    outDatumHash <-
      pmaybeDataC
        (fail "pDatumFromTxOut: no datum present in the output")
        pure
        (pfield @"datumHash" # txOut)
    datum <-
      pmaybeDataC
        (fail "pDatumFromTxOut: no datum with a given hash present in the transaction datums")
        pure
        (pfindDatum # getField @"datums" txInfo # outDatumHash)

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

pmustMintCurrency :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PInteger :--> PUnit)
pmustMintCurrency = phoistAcyclic $
  plam $ \ctx cs q -> unTermCont do
    ptraceC "mustMintCurrency"
    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["mint"] ctx'.txInfo
    mayMintQ <- pletC $ pvalueOfCurrency # cs # txInfo.mint
    pboolC
      (fail "mustMintCurrency: didn't mint the specified quantity")
      (ptraceC "mustMintCurrency: minted specified quantity" >> pure punit)
      (pdjust q #== mayMintQ)

pmustValidateAfter :: ClosedTerm (PScriptContext :--> PPOSIXTime :--> PUnit)
pmustValidateAfter = phoistAcyclic $
  plam $ \ctx after -> unTermCont do
    ptraceC "mustValidateAfter"
    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["validRange"] (getField @"txInfo" ctx')

    txValidRange <- pletC $ pfromData $ getField @"validRange" txInfo
    pboolC
      (fail "pmustValidateAfter: transaction validation range is not after 'after'")
      (ptraceC "pmustValidateAfter: transaction validation range is after 'after'" >> pure punit)
      (pbefore # after # txValidRange)

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

-- | Sums all token quantities for a given currency if it exists in the given value.
pvalueOfCurrency :: forall (q :: AmountGuarantees). ClosedTerm (PCurrencySymbol :--> PValue 'Sorted q :--> PMaybeData PInteger)
pvalueOfCurrency = phoistAcyclic $
  plam $ \cs val -> unTermCont do
    tokens <- pletC $ pto (pcurrencyTokens # cs # val)
    pboolC
      ( do
          ptraceC "pvalueOfCurrency: currency found"
          pure $ pdjust $ pfoldr # plam (#+) # 0 # (pmap # plam (\t -> pfromData $ psndBuiltin # t) # tokens)
      )
      (ptraceC "pvalueOfCurrency: currency not found" >> pure pdnothing)
      (tokens #== pnil)

-- | Checks and sums tokens spend from a given address
pmustSpendFromAddress :: ClosedTerm (PScriptContext :--> (PValue 'Sorted 'Positive :--> PMaybeData PInteger) :--> PAddress :--> PInteger)
pmustSpendFromAddress = phoistAcyclic $
  plam $ \ctx valPred addr -> unTermCont do
    ptraceC "mustSpendFromAddress"
    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["inputs"] ctx'.txInfo

    pure $
      pfoldl
        # plam
          ( \spent txInInfo -> unTermCont do
              resolved <- pletFieldsC @'["value", "address"] $ pfield @"resolved" # txInInfo
              mayValQ <- pletC $ valPred # resolved.value

              pmaybeDataC
                (pure spent)
                ( \q ->
                    pboolC
                      (fail "")
                      (pure $ spent #+ q)
                      (resolved.address #== addr)
                )
                mayValQ
          )
        # 0
        # pfromData txInfo.inputs

pdnothing :: Term s (PMaybeData a)
pdnothing = pcon $ PDNothing pdnil

pdjust :: PIsData a => Term s a -> Term s (PMaybeData a)
pdjust x = pcon $ PDJust $ pdcons # pdata x # pdnil
