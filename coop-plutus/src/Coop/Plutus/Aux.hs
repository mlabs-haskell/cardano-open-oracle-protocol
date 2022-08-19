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
  pflattenValue,
) where

import Control.Monad.Fail (MonadFail (fail))
import Plutarch (PType, TermCont, popaque, pto)
import Plutarch.Api.V1 (AmountGuarantees (NoGuarantees), KeyGuarantees (Sorted), PCurrencySymbol, PDatum, PDatumHash, PMap (PMap), PMaybeData (PDJust, PDNothing), PMintingPolicy, PScriptPurpose (PMinting), PTokenName, PTuple, PTxInInfo, PTxOutRef, PValue)
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V1.Value qualified as PValue
import Plutarch.Bool (PBool (PTrue))
import Plutarch.DataRepr (
  PDataRecord,
  PLabeledType ((:=)),
  pdcons,
 )
import Plutarch.Extra.TermCont (pletC, pletFieldsC, ptraceC)
import Plutarch.List (PIsListLike, PList, PListLike (pcons, pelimList, pnil), pany, pfoldl)
import Plutarch.Prelude (ClosedTerm, PAsData, PBool (PFalse), PBuiltinList, PBuiltinPair, PData, PEq ((#==)), PInteger, PIsData, PMaybe (PNothing), PTryFrom, PUnit, S, Term, getField, pcon, pconstant, pdata, pdnil, pfield, pfix, pfromData, pfstBuiltin, phoistAcyclic, pif, plam, plet, pmatch, psndBuiltin, ptraceError, ptryFrom, (#), (#$), type (:-->))
import Plutarch.TermCont (TermCont (runTermCont), tcont, unTermCont)
import Prelude (Applicative (pure), fst, ($), (<$>))

{- | Check if a 'PValue' contains the given currency symbol.
 NOTE: MangoIV says the plookup should be inlined here
-}
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

{-
    Minting policy for OneShot tokens.
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

-- TODO: Purge once part of a standard library
type FlattenedValue =
  ( PDataRecord
      '[ "currencySymbol" ':= PCurrencySymbol
       , "tokenName" ':= PTokenName
       , "amount" ':= PInteger
       ]
  )

-- | Converts a given value into a "flatten" list representation.
pflattenValue ::
  forall {s :: S}.
  Term s (PValue 'Sorted 'NoGuarantees :--> PList FlattenedValue)
pflattenValue = phoistAcyclic $
  plam $ \v ->
    let v' = pto v
     in pfoldWithKey # appendCurrencySymbolTokens # pnil # v'
  where
    appendCurrencySymbolTokens ::
      forall {s :: S}.
      Term
        s
        ( PList FlattenedValue
            :--> PCurrencySymbol
            :--> PMap 'Sorted PTokenName PInteger
            :--> PList FlattenedValue
        )
    appendCurrencySymbolTokens = phoistAcyclic $
      plam $ \acc cs ts ->
        pfoldWithKey # (appendToken # cs) # acc # ts

    appendToken ::
      forall {s :: S}.
      Term
        s
        ( PCurrencySymbol
            :--> PList FlattenedValue
            :--> PTokenName
            :--> PInteger
            :--> PList FlattenedValue
        )
    appendToken = phoistAcyclic $
      plam $ \cs acc tn i ->
        let flattened =
              pdcons @"currencySymbol" @PCurrencySymbol
                # pdata cs
                #$ pdcons @"tokenName" @PTokenName
                # pdata tn
                #$ pdcons @"amount" @PInteger
                # pdata i
                #$ pdnil
         in pcons # flattened # acc

    pfoldWithKey ::
      forall
        {s :: S}
        (k :: PType)
        (v :: PType)
        (b :: PType)
        (w :: KeyGuarantees).
      (PIsData k, PIsData v) =>
      Term s ((b :--> k :--> v :--> b) :--> b :--> PMap w k v :--> b)
    pfoldWithKey = phoistAcyclic $
      plam $ \f e m ->
        pfoldlWithKey' # f # e #$ pgetMapList # m
      where
        pfoldlWithKey' ::
          forall {s :: S} (k :: PType) (v :: PType) (b :: PType).
          (PIsData k, PIsData v) =>
          Term
            s
            ( (b :--> k :--> v :--> b)
                :--> b
                :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
                :--> b
            )
        pfoldlWithKey' = phoistAcyclic $
          plam $ \f e xs ->
            pfoldl # (go # f) # e # xs
          where
            go ::
              forall {s :: S} (k :: PType) (v :: PType) (b :: PType).
              (PIsData k, PIsData v) =>
              Term
                s
                ( (b :--> k :--> v :--> b)
                    :--> b
                    :--> (PBuiltinPair (PAsData k) (PAsData v) :--> b)
                )
            go = phoistAcyclic $
              plam $ \f e pair ->
                f # e # pfromData (pfstBuiltin # pair)
                  # pfromData (psndBuiltin # pair)

        pgetMapList ::
          forall {s :: S} (k :: PType) (v :: PType) (w :: KeyGuarantees).
          Term
            s
            ( PMap w k v
                :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
            )
        pgetMapList = phoistAcyclic $
          plam $ \m -> pmatch m $ \case
            PMap aList -> aList
