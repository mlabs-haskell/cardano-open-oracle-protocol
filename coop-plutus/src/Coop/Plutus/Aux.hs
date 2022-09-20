{-# LANGUAGE BlockArguments #-}

module Coop.Plutus.Aux (
  phasCurrency,
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
  pmustPayCurrencyWithDatumTo,
  pfindOwnInput',
  pfoldTxOutputs,
  pfoldTxInputs,
  pmustHandleSpentWithMp,
  pcurrencyValue,
  pmustSpendAtLeast,
  pmaybeData,
  hashTxInputs,
  pmustMintCurrency,
  pcurrencyTokenQuantity,
) where

import Crypto.Hash (SHA3_256 (SHA3_256), hashWith)
import Data.ByteArray (convert)
import Data.ByteString (ByteString, cons)
import Data.List (sort, zipWith)
import Plutarch (popaque, pto)
import Plutarch.Api.V1.AssocMap (pempty, plookup, psingleton)
import Plutarch.Api.V1.Value (pnoAdaValue, pnormalize, pvalueOf)
import Plutarch.Api.V1.Value qualified as PValue
import Plutarch.Api.V2 (AmountGuarantees (NonZero), KeyGuarantees (Sorted), PAddress, PCurrencySymbol, PDatum, PDatumHash, PExtended, PInterval (PInterval), PLowerBound (PLowerBound), PMap (PMap), PMaybeData (PDJust, PDNothing), PMintingPolicy, POutputDatum (PNoOutputDatum, POutputDatum, POutputDatumHash), PPOSIXTime, PPubKeyHash, PScriptContext, PScriptPurpose (PMinting, PSpending), PTokenName, PTuple, PTxInInfo, PTxOut, PTxOutRef, PUpperBound, PValue (PValue))
import Plutarch.Bool (PBool (PTrue))
import Plutarch.DataRepr (pdcons)
import Plutarch.Extra.Interval (pcontains)
import Plutarch.List (PIsListLike, PListLike (pelimList, pnil), pany)
import Plutarch.Monadic qualified as P
import Plutarch.Num (PNum ((#+)))
import Plutarch.Prelude (ClosedTerm, PAsData, PBool (PFalse), PBuiltinList (PCons, PNil), PData, PEq ((#==)), PInteger (), PIsData, PMaybe (PJust, PNothing), PPartialOrd ((#<=)), PTryFrom, PUnit, S, Term, getField, pcon, pconstant, pconstantData, pdata, pdnil, pelem, pfield, pfind, pfix, pfoldl, pfromData, pfstBuiltin, phoistAcyclic, pif, plam, plet, pletFields, pmap, pmatch, psndBuiltin, ptrace, ptraceError, ptryFrom, (#), (#$), (#&&), type (:-->))
import Plutarch.TermCont (tcont, unTermCont)
import PlutusLedgerApi.V2 (Extended (PosInf), TxId (getTxId), TxInInfo (TxInInfo), TxOutRef (txOutRefId, txOutRefIdx), UpperBound (UpperBound), fromBuiltin)
import Prelude (Bool (False, True), Functor (fmap), Monoid (mconcat, mempty), Num (fromInteger), Semigroup ((<>)), fst, reverse, ($), (.), (<$>))

-- WARN[Andrea]: on a 'NoGuarantees value like `v = psingleton # cs # tn # 0` we have
--               `phasCurrency # cs # tn # v #== pcon PTrue` which could be misleading.
--               For values from `TxInfo` the only case where this is relevant is if `v` is the mint field and cs:tk is Ada.
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

-- | Retrieves a Value of a specified CurrencySymbol or fails otherwise
pcurrencyValue :: forall (q :: AmountGuarantees) (s :: S). Term s (PCurrencySymbol :--> PValue 'Sorted q :--> PValue 'Sorted 'NonZero)
pcurrencyValue = phoistAcyclic $
  plam $ \cs val ->
    ptrace "pcurrencyValue" $
      pmatch
        (plookup # cs # pto val)
        ( \case
            PNothing -> ptraceError "pcurrencyValue: Must have a specified CurrencySymbol in the Value"
            PJust tokens -> pnormalize # pcon (PValue $ psingleton # cs # tokens)
        )

-- | Retrieves a quantity of a specified AssetClass only if it's a singleton or fails otherwise
pcurrencyTokenQuantity :: forall (q :: AmountGuarantees) (s :: S). Term s (PCurrencySymbol :--> PTokenName :--> PValue 'PValue.Sorted q :--> PInteger)
pcurrencyTokenQuantity = phoistAcyclic $
  plam $ \cs tn val ->
    ptrace "pcurrencyTokenQuantity" $
      pmatch
        (plookup # cs # pto val)
        ( \case
            PNothing -> ptraceError "pcurrencyTokenQuantity: Must have a specified CurrencySymbol in the Value"
            PJust tokens -> pmatch
              (pto tokens)
              \case
                PNil -> ptraceError "pcurrencyTokenQuantity: Must have a specified TokenName in the Value under a specified CurrencySymbol"
                PCons entry rest ->
                  pif
                    (rest #== pnil #&& pfromData (pfstBuiltin # entry) #== tn)
                    (pfromData $ psndBuiltin # entry)
                    (ptraceError "pcurrencyTokenQuantity: Must have only as single Token Name under a specified CurrencySymbol")
        )

-- PERF[Andrea]: I believe `PMaybe a` is more efficient to interact
-- with and PMaybeData is mostly useful for UTxO datums.
pmaybeData :: PIsData a => Term s (PMaybeData a) -> Term s b -> (Term s a -> Term s b) -> Term s b
pmaybeData m l r = pmatch m \case
  PDNothing _ -> l
  PDJust x -> r (pfield @"_0" # x)

punit :: Term s PUnit
punit = pconstant ()

ptryFromData :: forall a s. PTryFrom PData (PAsData a) => Term s PData -> Term s (PAsData a)
ptryFromData x = unTermCont $ fst <$> tcont (ptryFrom @(PAsData a) x)

pownCurrencySymbol :: Term s (PScriptPurpose :--> PCurrencySymbol)
pownCurrencySymbol = phoistAcyclic $
  plam $ \purpose -> ptrace "pownCurrencySymbol" $
    pmatch purpose \case
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
  plam $ \ctx -> ptrace "pfindOwnInput'" P.do
    ctx' <- pletFields @'["txInfo", "purpose"] ctx
    txInfo <- pletFields @'["inputs"] ctx'.txInfo
    pmatch ctx'.purpose \case
      PSpending txOutRef ->
        pmatch
          (pfindOwnInputV2 # txInfo.inputs # (pfield @"_0" # txOutRef))
          \case
            PNothing -> ptraceError "pfindOwnInput': Script purpose is not 'Spending'!"
            PJust txInInfo -> txInInfo
      _ -> ptraceError "pfindOwnInput': Script purpose is not 'Spending'!"

-- | Find the data corresponding to a data hash, if there is one
pfindDatum :: Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum)) :--> PDatumHash :--> PMaybeData PDatum)
pfindDatum = phoistAcyclic $
  plam $ \datums dh ->
    ptrace "pfindDatum" pfindMap
      # plam
        ( \pair -> P.do
            pair' <- pletFields @'["_0", "_1"] $ pfromData pair
            dh' <- plet $ getField @"_0" pair'
            datum <- plet $ getField @"_1" pair'
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
  plam $ \q tn txOutRef _ ctx -> ptrace "oneShotMp" P.do
    ctx' <- pletFields @'["txInfo", "purpose"] ctx
    txInfo <- pletFields @'["inputs", "mint"] ctx'.txInfo
    inputs <- plet $ pfromData txInfo.inputs
    mint <- plet $ pfromData $ txInfo.mint
    cs <- plet $ pownCurrencySymbol # ctx'.purpose

    _ <-
      plet $
        pif
          (pconsumesRef # pfromData txOutRef # inputs)
          (ptrace "oneShotMp: Consumes the specified outref" punit)
          (ptraceError "oneShotMp: Must consume the specified utxo")

    -- WARN[Andrea]: allows minting/burning other token names too.
    --               although the whole check is maybe redundant since this can only run once?
    pif
      (pvalueOf # mint # cs # pfromData tn #== pfromData q)
      (ptrace "oneShotMp: Mints the specified quantity of tokens" $ popaque punit)
      (ptraceError "oneShotMp: Must mint the specified quantity of tokens")

-- | Check if utxo is consumed
pconsumesRef :: Term s (PTxOutRef :--> PBuiltinList PTxInInfo :--> PBool)
pconsumesRef = phoistAcyclic $
  plam $ \txOutRef ->
    pany #$ plam $ \input -> pfield @"outRef" # input #== txOutRef

pdatumFromTxOut :: forall a (s :: S). (PIsData a, PTryFrom PData (PAsData a)) => Term s (PScriptContext :--> PTxOut :--> a)
pdatumFromTxOut = phoistAcyclic $
  plam $ \ctx txOut -> ptrace "pdatumFromTxOut" P.do
    -- TODO: Migrate to inline datums
    ctx' <- pletFields @'["txInfo"] ctx
    txInfo <- pletFields @'["datums"] ctx'.txInfo

    datum <- plet $ pmatch (pfield @"datum" # txOut) \case
      PNoOutputDatum _ -> ptraceError "pDatumFromTxOut: Must have a datum present in the output"
      POutputDatumHash r -> ptrace "pDatumFromTxOut: Got a datum hash" P.do
        pmatch (plookup # pfromData (pfield @"datumHash" # r) # txInfo.datums) \case
          PNothing -> ptraceError "pDatumFromTxOut: Datum with a given hash must be present in the transaction datums"
          PJust datum -> ptrace "pDatumFromTxOut: Found a datum" datum
      POutputDatum r -> ptrace "pDatumFromTxOut: Got an inline datum" $ pfield @"outputDatum" # r

    pfromData (ptryFromData @a (pto datum))

-- WARN[Andrea]: leaves you vulnerable to `other tokenname` attacks.
pmustMint :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PTokenName :--> PInteger :--> PUnit)
pmustMint = phoistAcyclic $
  plam $ \ctx cs tn q -> ptrace "mustMint" P.do
    ctx' <- pletFields @'["txInfo"] ctx
    txInfo <- pletFields @'["mint"] ctx'.txInfo
    pif
      (pvalueOf # txInfo.mint # cs # tn #== q)
      (ptrace "pmustMint: Minted specified quantity" punit)
      (ptraceError "pmustMint: Must mint the specified quantity")

-- | Checks total value of a specified CurrencySymbol minted.
pmustMintCurrency :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PValue 'Sorted 'NonZero :--> PUnit)
pmustMintCurrency = phoistAcyclic $
  plam $ \ctx cs val -> ptrace "pmustMintCurrency" P.do
    ctx' <- pletFields @'["txInfo"] ctx
    txInfo <- pletFields @'["mint"] ctx'.txInfo
    pif
      (pcurrencyValue # cs # txInfo.mint #== val)
      (ptrace "pmustMintCurrency: Minted specified token name and quantity exclusively" punit)
      (ptraceError "pmustMintCurrency: Must mint the specified token name exclusively")

pmustValidateAfter :: ClosedTerm (PScriptContext :--> PExtended PPOSIXTime :--> PUnit)
pmustValidateAfter = phoistAcyclic $
  plam $ \ctx after -> ptrace "mustValidateAfter" P.do
    ctx' <- pletFields @'["txInfo"] ctx
    txInfo <- pletFields @'["validRange"] (getField @"txInfo" ctx')

    txValidRange <- plet $ pfromData $ getField @"validRange" txInfo
    pif
      (pcontains # (pinterval' # pdata (plowerBound # after) # pdata pposInf) # txValidRange)
      (ptrace "pmustValidateAfter: Transaction validation range is after 'after'" punit)
      (ptraceError "pmustValidateAfter: Transaction validation range must come after 'after'")

-- | Interval from upper and lower bounds.
pinterval' ::
  forall a (s :: S).
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
  plam $ \ctx pkh -> ptrace "mustBeSignedBy" P.do
    ctx' <- pletFields @'["txInfo"] ctx
    txInfo <- pletFields @'["signatories"] (getField @"txInfo" ctx')
    sigs <- plet $ getField @"signatories" txInfo
    pif
      (pelem # pdata pkh # sigs)
      (ptrace "mustBeSignedBy: Specified pkh signed the transaction" punit)
      (ptraceError "mustBeSignedBy: Specified pkh must sign the transaction")

-- | Foldl over transaction outputs.
pfoldTxOutputs :: ClosedTerm (PScriptContext :--> (a :--> PTxOut :--> a) :--> a :--> a)
pfoldTxOutputs = phoistAcyclic $
  plam $ \ctx foldFn initial -> ptrace "pfoldTxInputs" P.do
    ctx' <- pletFields @'["txInfo"] ctx
    txInfo <- pletFields @'["outputs"] ctx'.txInfo

    pfoldl
      # foldFn
      # initial
      # pfromData txInfo.outputs

-- | Checks total value of a specified CurrencySymbol paid to an Address and checks whether attached datums are valid.
pmustPayCurrencyWithDatumTo :: forall a. (PIsData a, PTryFrom PData (PAsData a)) => ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PValue 'Sorted 'NonZero :--> (a :--> PBool) :--> PAddress :--> PUnit)
pmustPayCurrencyWithDatumTo = phoistAcyclic $
  plam $ \ctx cs mustPayVal datumPred addr -> ptrace "pmustPayCurrencyWithDatumTo" P.do
    let foldFn paid txOut = P.do
          txOut' <- pletFields @'["value", "address"] txOut

          pif
            ((txOut'.address) #== pdata addr)
            ( P.do
                dat <- plet $ pdatumFromTxOut @a # ctx # txOut
                pif
                  (datumPred # dat)
                  (ptrace "pmustPayCurrencyWithDatumTo: Valid datum attached" (paid <> (pcurrencyValue # cs # txOut'.value)))
                  (ptraceError "pmustPayCurrencyWithDatumTo: Must attach a valid datum")
            )
            paid

    paidVal <-
      plet $
        pfoldTxOutputs # ctx
          # plam foldFn
          # mempty

    pif
      (mustPayVal #== paidVal)
      (ptrace "pmustPayExWithDatumTo: Paid the specified value" punit)
      (ptraceError "pmustPayExWithDatumTo: Must pay the specified value")

-- | Foldl over transaction inputs
pfoldTxInputs :: ClosedTerm (PScriptContext :--> (a :--> PTxInInfo :--> a) :--> a :--> a)
pfoldTxInputs = phoistAcyclic $
  plam $ \ctx foldFn initial -> ptrace "pfoldTxInputs" P.do
    ctx' <- pletFields @'["txInfo"] ctx
    txInfo <- pletFields @'["inputs"] ctx'.txInfo

    pfoldl
      # foldFn
      # initial
      # pfromData txInfo.inputs

-- | Checks total tokens spent
pmustSpendPred :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PTokenName :--> (PInteger :--> PBool) :--> PUnit)
pmustSpendPred = phoistAcyclic $
  plam $ \ctx cs tn predOnQ -> ptrace "pmustSpendPred" P.do
    spentQ <-
      plet $
        pfoldTxInputs # ctx
          # plam
            ( \spent txInInfo -> P.do
                resolved <- pletFields @'["value"] $ pfield @"resolved" # txInInfo
                spent #+ (pvalueOf # resolved.value # cs # tn)
            )
          # 0

    pif
      (predOnQ # spentQ)
      (ptrace "pmustSpendPred: Spent required quantity" punit)
      (ptraceError "pmustSpendPred: Must spend the required quantity")

-- | Checks total tokens spent
pmustSpend :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PTokenName :--> PInteger :--> PUnit)
pmustSpend = phoistAcyclic $
  plam $ \ctx cs tn mustSpendQ -> pmustSpendPred # ctx # cs # tn # plam (#== mustSpendQ)

-- | Checks total tokens spent
pmustSpendAtLeast :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PTokenName :--> PInteger :--> PUnit)
pmustSpendAtLeast = phoistAcyclic $
  plam $ \ctx cs tn mustSpendAtLeastQ -> pmustSpendPred # ctx # cs # tn # plam (mustSpendAtLeastQ #<=)

-- WARN[Andrea]: this is very permissive, maybe you should have
-- different versions that check for either burning or minting the tokens.
pmustHandleSpentWithMp :: ClosedTerm (PScriptContext :--> PUnit)
pmustHandleSpentWithMp = phoistAcyclic $
  plam $ \ctx -> ptrace "pmustHandleSpentWithMp" P.do
    ctx' <- pletFields @'["txInfo"] ctx
    txInfo <- pletFields @'["mint"] ctx'.txInfo
    mint <- plet $ pto $ pfromData txInfo.mint

    ownIn <- plet $ pfindOwnInput' # ctx
    ownInVal <- plet $ pnoAdaValue #$ pfield @"value" # (pfield @"resolved" # ownIn)
    _ <- plet $ pmatch (pto ownInVal) \case
      PMap elems ->
        pmap
          # plam
            ( \kv -> P.do
                cs <- plet $ pfromData $ pfstBuiltin # kv
                pmatch (plookup # cs # mint) \case
                  PNothing -> ptraceError "pmustHandleSpentWithMp: Spent currency symbol must be in mint"
                  PJust _ -> ptrace "pmustHandleSpentWithMp: Spent currency symbol found in mint" punit
            )
          # elems
    ptrace "pmustHandleSpentWithMp: All spent currency symbols are in mint" punit

pdnothing :: Term s (PMaybeData a)
pdnothing = pcon $ PDNothing pdnil

pdjust :: PIsData a => Term s a -> Term s (PMaybeData a)
pdjust x = pcon $ PDJust $ pdcons # pdata x # pdnil

hashTxInputs :: [TxInInfo] -> ByteString
hashTxInputs inputs =
  let orefs = [oref | TxInInfo oref _ <- inputs]
      sortedOrefs = Prelude.reverse $ sort orefs -- TODO: Why the reverse works?
      ixs = fmap (fromInteger . txOutRefIdx) sortedOrefs
      txIds = fmap (fromBuiltin . getTxId . txOutRefId) sortedOrefs
      hashedOref = convert @_ @ByteString . hashWith SHA3_256 . mconcat $ zipWith cons ixs txIds
   in hashedOref
