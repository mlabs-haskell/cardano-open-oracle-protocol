module Coop.Validation.Aux (
  phasCurrency,
  punit,
  ptryFromData,
  pownCurrencySymbol,
  pfindDatum,
  mkOneShotMp,
  pfindMap,
  pdatumFromTxOut,
  pmustValidateAfter,
  pmustBeSignedBy,
  pdjust,
  pdnothing,
  pmustSpend,
  pmustPayCurrencyWithDatumTo,
  pfindOwnInput',
  pfoldTxOutputs,
  pfoldTxInputs,
  pcurrencyValue,
  pmustSpendAtLeast,
  pmaybeData,
  hashTxInputs,
  pmustMintCurrency,
  pcurrencyTokenQuantity,
  pfindOwnInput,
  pfindOwnAddr,
  pmustBurnOwnSingletonValue,
  pfoldTxRefs,
  pdpair,
  punwords,
  plookupSymbol,
  pvalueWithoutAda,
  plistSingleton,
  pqtokenName,
  pqtokenName',
  passetClassValue,
  pgetTokenNameIfSingle,
)
where

import Crypto.Hash (Blake2b_256 (Blake2b_256), hashWith)
import Data.ByteArray (convert)
import Data.ByteString (ByteString, cons)
import Data.List (intersperse, sort, zipWith)
import LambdaBuffers.Runtime.Plutarch (PAssetClass)
import Plutarch (popaque, pto)
import Plutarch.Api.V1.AssocMap (PMap (PMap), plookup, psingleton)
import Plutarch.Api.V1.Tuple (pbuiltinPairFromTuple, ptuple)
import Plutarch.Api.V1.Value (
  AmountGuarantees (Positive),
  padaSymbol,
  pnoAdaValue,
  pnormalize,
  pvalueOf,
 )
import Plutarch.Api.V1.Value qualified as PValue
import Plutarch.Api.V2 (AmountGuarantees (NonZero), KeyGuarantees (Sorted), PAddress, PCurrencySymbol, PDatum, PDatumHash, PExtended, PInterval (PInterval), PLowerBound (PLowerBound), PMaybeData (PDJust, PDNothing), PMintingPolicy, POutputDatum (PNoOutputDatum, POutputDatum, POutputDatumHash), PPOSIXTime, PPubKeyHash, PScriptContext, PScriptPurpose (PMinting, PSpending), PTokenName (PTokenName), PTuple, PTxInInfo, PTxOut, PTxOutRef, PUpperBound, PValue (PValue))
import Plutarch.Bool (PBool (PTrue), pnot)
import Plutarch.DataRepr (pdcons)
import Plutarch.Extra.Interval (pcontains)
import Plutarch.Extra.TermCont (pguardC, pletC, pmatchC)
import Plutarch.Lift (PUnsafeLiftDecl)
import Plutarch.List (PIsListLike, PListLike (pelimList, pnil), pany)
import Plutarch.Monadic qualified as P
import Plutarch.Num (PNum (pnegate, (#+)))
import Plutarch.Prelude (ClosedTerm, PAsData, PBool (PFalse), PBuiltinList (PCons, PNil), PBuiltinPair, PByteString, PData, PEq ((#==)), PInteger (), PIsData, PMaybe (PJust, PNothing), PPartialOrd ((#<=)), PString, PTryFrom, PUnit, S, Term, getField, pcon, pconstant, pconstantData, pdata, pdnil, pelem, pfield, pfilter, pfind, pfix, pfoldl, pfromData, pfstBuiltin, phoistAcyclic, pif, plam, plet, pletFields, pmatch, pshow, psndBuiltin, ptrace, ptraceError, ptryFrom, (#), (#$), (#&&), type (:-->))
import Plutarch.TermCont (tcont, unTermCont)
import PlutusLedgerApi.V2 (Extended (PosInf), TxId (getTxId), TxInInfo (TxInInfo), TxOutRef (txOutRefId, txOutRefIdx), UpperBound (UpperBound), fromBuiltin)
import Prelude (Applicative (pure), Bool (False, True), Functor (fmap), Monoid (mconcat, mempty), Num (fromInteger), Semigroup ((<>)), fst, ($), (.), (<$>))

-- | Checks if there's a CurrencySymbol in a Value
phasCurrency :: forall (s :: S). Term s (PCurrencySymbol :--> PValue 'Sorted 'Positive :--> PBool)
phasCurrency = phoistAcyclic $
  plam $ \cs val ->
    pmatch
      (plookup # cs # pto val)
      ( \case
          PNothing -> pcon PFalse
          _ -> pcon PTrue
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

-- | Deconstruct a PMaybeData type
pmaybeData :: PIsData a => Term s (PMaybeData a) -> Term s b -> (Term s a -> Term s b) -> Term s b
pmaybeData m l r = pmatch m \case
  PDNothing _ -> l
  PDJust x -> r (pfield @"_0" # x)

pdnothing :: Term s (PMaybeData a)
pdnothing = pcon $ PDNothing pdnil

pdjust :: PIsData a => Term s a -> Term s (PMaybeData a)
pdjust x = pcon $ PDJust $ pdcons # pdata x # pdnil

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

pfindOwnAddr :: Term s (PScriptContext :--> PAddress)
pfindOwnAddr = phoistAcyclic $
  plam $ \ctx -> ptrace "pfindOwnAddr" P.do
    ownInput <- plet $ pfindOwnInput # ctx
    pfield @"address" #$ pfield @"resolved" # ownInput

pfindOwnInput :: Term s (PScriptContext :--> PTxInInfo)
pfindOwnInput = phoistAcyclic $
  plam $ \ctx -> ptrace "pfindOwnInput" P.do
    ctx' <- pletFields @'["txInfo", "purpose"] ctx
    txInfo <- pletFields @'["inputs"] ctx'.txInfo
    pmatch ctx'.purpose \case
      PSpending txOutRef ->
        pmatch
          (pfindOwnInput' # txInfo.inputs # (pfield @"_0" # txOutRef))
          \case
            PNothing -> ptraceError "pfindOwnInput: Script purpose is not 'Spending'!"
            PJust txInInfo -> txInInfo
      _ -> ptraceError "pfindOwnInput: Script purpose is not 'Spending'!"

pfindOwnInput' :: Term s (PBuiltinList PTxInInfo :--> PTxOutRef :--> PMaybe PTxInInfo)
pfindOwnInput' = phoistAcyclic $
  plam $ \inputs outRef ->
    pfind # (matches # outRef) # inputs
  where
    matches :: Term s (PTxOutRef :--> PTxInInfo :--> PBool)
    matches = phoistAcyclic $
      plam $ \outref txininfo ->
        outref #== pfield @"outRef" # txininfo

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

{- | Minting policy for OneShot tokens

 - check a given `TxOutRef` is consumed
 - check `q` $ONE-SHOT tokens are minted

 Notes:

 - guarantees $ONE-SHOT tokens are only minted once
-}
mkOneShotMp ::
  ClosedTerm
    ( PAsData PInteger
        :--> PAsData PTokenName
        :--> PAsData PTxOutRef
        :--> PMintingPolicy
    )
mkOneShotMp = phoistAcyclic $
  plam $ \q tn txOutRef _ ctx -> ptrace "oneShotMp" P.do
    ctx' <- pletFields @'["txInfo", "purpose"] ctx
    txInfo <- pletFields @'["inputs", "mint"] ctx'.txInfo
    inputs <- plet $ pfromData txInfo.inputs
    cs <- plet $ pownCurrencySymbol # ctx'.purpose

    _ <-
      plet $
        pif
          (pconsumesRef # pfromData txOutRef # inputs)
          (ptrace "oneShotMp: Consumes the specified outref" punit)
          (ptraceError "oneShotMp: Must consume the specified utxo")

    _ <- plet $ pmustMintCurrency # ctx # cs # (PValue.psingleton # cs # pfromData tn # pfromData q)

    ptrace "oneShotMp: Mints the specified quantity of tokens" $ popaque punit

-- | Check if utxo is consumed
pconsumesRef :: Term s (PTxOutRef :--> PBuiltinList PTxInInfo :--> PBool)
pconsumesRef = phoistAcyclic $
  plam $ \txOutRef ->
    pany #$ plam $ \input -> pfield @"outRef" # input #== txOutRef

-- | Parses a datum from a TxOut or fails hard
pdatumFromTxOut :: forall a (s :: S). (PIsData a, PTryFrom PData (PAsData a)) => Term s (PScriptContext :--> PTxOut :--> a)
pdatumFromTxOut = phoistAcyclic $
  plam $ \ctx txOut -> ptrace "pdatumFromTxOut" P.do
    datum <- plet $ pmatch (pfield @"datum" # txOut) \case
      PNoOutputDatum _ -> ptraceError "pDatumFromTxOut: Must have a datum present in the output"
      POutputDatumHash r -> ptrace "pDatumFromTxOut: Got a datum hash" P.do
        ctx' <- pletFields @'["txInfo"] ctx
        txInfo <- pletFields @'["datums"] ctx'.txInfo
        pmatch (plookup # pfromData (pfield @"datumHash" # r) # txInfo.datums) \case
          PNothing -> ptraceError "pDatumFromTxOut: Datum with a given hash must be present in the transaction datums"
          PJust datum -> ptrace "pDatumFromTxOut: Found a datum" datum
      POutputDatum r -> ptrace "pDatumFromTxOut: Got an inline datum" $ pfield @"outputDatum" # r

    pfromData (ptryFromData @a (pto datum))

-- | Checks total value of a specified CurrencySymbol minted.
pmustMintCurrency :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PValue 'Sorted 'NonZero :--> PUnit)
pmustMintCurrency = phoistAcyclic $
  plam $ \ctx cs val -> ptrace "pmustMintCurrency" P.do
    ctx' <- pletFields @'["txInfo"] ctx
    txInfo <- pletFields @'["mint"] ctx'.txInfo

    pif
      (pcurrencyValue # cs # txInfo.mint #== val)
      (ptrace "pmustMintCurrency: Minted specified value of currency exclusively" punit)
      (ptraceError "pmustMintCurrency: Must mint the specified value of currency")

-- | Checks that the transaction validates after the specified point
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
        pdcons @"from"
          # lower
          #$ pdcons @"to"
          # upper
          # pdnil

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
  plam $ \ctx foldFn initial -> ptrace "pfoldTxOutputs" P.do
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
        pfoldTxOutputs
          # ctx
          # plam foldFn
          # mempty

    pif
      (mustPayVal #== paidVal)
      (ptrace "pmustPayExWithDatumTo: Paid the specified value" punit)
      (ptraceError "pmustPayExWithDatumTo: Must pay the specified value")

-- | Foldl over transaction references
pfoldTxRefs :: ClosedTerm (PScriptContext :--> (a :--> PTxInInfo :--> a) :--> a :--> a)
pfoldTxRefs = phoistAcyclic $
  plam $ \ctx foldFn initial -> ptrace "pfoldTxRefs" P.do
    ctx' <- pletFields @'["txInfo"] ctx
    txInfo <- pletFields @'["referenceInputs"] ctx'.txInfo

    pfoldl
      # foldFn
      # initial
      # pfromData txInfo.referenceInputs

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
        pfoldTxInputs
          # ctx
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
  plam $
    \ctx cs tn mustSpendQ -> pmustSpendPred # ctx # cs # tn # plam (#== mustSpendQ)

-- | Checks total tokens spent
pmustSpendAtLeast :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PTokenName :--> PInteger :--> PUnit)
pmustSpendAtLeast = phoistAcyclic $
  plam $
    \ctx cs tn mustSpendAtLeastQ -> pmustSpendPred # ctx # cs # tn # plam (mustSpendAtLeastQ #<=)

{- | Must spend and burn 'own' singleton AssetClass

 - check that the 'own' non $ADA token spent is a singleton
 - check that the quantity is entirely burned

 WARN: Use this function only when the input contains the entire quantity of an AssetClass!!!

 Attack scenario:

 - only 2 x assetClass "mintingPolicyA" "someTokenName" are minted and paid to legitValidator in 2 different UTxOs
 - legitValidator validates spending using this function
 - Alice creates a transaction that spends both UTxOs containins these tokens BUT
  - burns only one of them
  - pays to somewhere else the other one
-}
pmustBurnOwnSingletonValue :: ClosedTerm (PScriptContext :--> PUnit)
pmustBurnOwnSingletonValue = phoistAcyclic $
  plam $ \ctx -> ptrace "pmustSinkhole" P.do
    ctx' <- pletFields @'["txInfo"] ctx
    txInfo <- pletFields @'["mint"] ctx'.txInfo

    ownInput <- plet $ pfindOwnInput # ctx
    ownInValue <- plet $ pnoAdaValue #$ pfield @"value" # (pfield @"resolved" # ownInput)

    pmatch
      (pto . pto $ ownInValue)
      \case
        PNil -> ptraceError "Must spend at least 1 non-ADA token"
        PCons csTokens restCsTokens -> ptrace "Spent at least 1 non-ADA token" P.do
          pif
            (restCsTokens #== pnil)
            ( ptrace "Spent a single CurrencySymbol" P.do
                pmatch (pto . pfromData $ psndBuiltin # csTokens) \case
                  PNil -> ptraceError "Must spend at least 1 token of CurrencySymbol"
                  PCons tokenQ restTokenQs ->
                    pif
                      (restTokenQs #== pnil)
                      ( ptrace "Spent a single TokenName" P.do
                          cs <- plet . pfromData $ pfstBuiltin # csTokens
                          tn <- plet . pfromData $ pfstBuiltin # tokenQ
                          q <- plet . pfromData $ psndBuiltin # tokenQ

                          pif
                            (pnegate # q #== pvalueOf # txInfo.mint # cs # tn)
                            (ptrace "Burned the same quantity that was spent" punit)
                            (ptraceError "Must burn the same quantity that was spent")
                      )
                      (ptraceError "Must spend a single TokenName")
            )
            (ptraceError "Must spent a single CurrencySymbol")

-- | Hashes transaction inputs blake2b_256 on the concatenation of id:ix (used for onchain uniqueness)
hashTxInputs :: [TxInInfo] -> ByteString
hashTxInputs inputs =
  let orefs = [oref | TxInInfo oref _ <- inputs]
      sortedOrefs = sort orefs
      ixs = fmap (fromInteger . txOutRefIdx) sortedOrefs
      txIds = fmap (fromBuiltin . getTxId . txOutRefId) sortedOrefs
      hashedOref = convert @_ @ByteString . hashWith Blake2b_256 . mconcat $ zipWith cons ixs txIds
   in hashedOref

pdpair :: PIsData a => PIsData b => Term s a -> Term s b -> Term s (PBuiltinPair (PAsData a) (PAsData b))
pdpair l r = pfromData . pbuiltinPairFromTuple . pdata $ ptuple # pdata l # pdata r

punwords ::
  forall (s :: S).
  [Term s PString] ->
  Term s PString
punwords = mconcat . intersperse " "

plookupSymbol :: forall (g :: AmountGuarantees). ClosedTerm (PValue 'Sorted g :--> PCurrencySymbol :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
plookupSymbol = phoistAcyclic $ plam \val sym -> unTermCont $ do
  PValue valueMap <- pmatchC val
  PJust tnNamexQuantities <- pmatchC $ plookup # sym # valueMap
  PMap tnNamexQuantities' <- pmatchC tnNamexQuantities
  pure tnNamexQuantities'

pvalueWithoutAda :: ClosedTerm (PValue s g :--> PValue s g)
pvalueWithoutAda = phoistAcyclic $ plam $ \v ->
  pcon $ PValue $ pcon $ PMap $ pfilter # plam (\x -> pnot # (pfromData (pfstBuiltin # x) #== padaSymbol)) # pto (pto v)

plistSingleton :: PUnsafeLiftDecl a => ClosedTerm (a :--> PBuiltinList a)
plistSingleton = phoistAcyclic $ plam \x -> pcon $ PCons x (pcon PNil)

-- | `pqtokenName' tokenNameBytes quantity` same as `pqtokenName` but receives bytes instead of token name
pqtokenName' :: ClosedTerm (PByteString :--> PInteger :--> PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
pqtokenName' = phoistAcyclic $ plam \tokenNameBytes quantity -> pqtokenName # pcon (PTokenName tokenNameBytes) # quantity

-- | `pqtokenName tokenName quantity` aka 'quantified token name' creates a pair of token name and integer
pqtokenName :: ClosedTerm (PTokenName :--> PInteger :--> PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
pqtokenName = phoistAcyclic $ plam pdpair

passetClassValue :: ClosedTerm (PAssetClass :--> PInteger :--> PValue 'Sorted 'NonZero)
passetClassValue = phoistAcyclic $ plam \asset quantity -> PValue.psingleton # (pfield @"_0" # asset) # (pfield @"_1" # asset) # quantity

pgetTokenNameIfSingle :: ClosedTerm (PValue 'Sorted g :--> PCurrencySymbol :--> PTokenName)
pgetTokenNameIfSingle = phoistAcyclic $ plam \value symbol -> unTermCont $ do
  PCons qtokenName rest <- pmatchC $ plookupSymbol # value # symbol
  gotQuantity <- pletC . pfromData $ psndBuiltin # qtokenName
  pguardC (punwords ["Wanted 1 but got", pshow gotQuantity]) $
    gotQuantity #== pconstant 1
  pguardC (punwords ["Expected only a single quantified token name but got", pshow rest]) $
    rest #== pcon PNil
  pure $ pfromData $ pfstBuiltin # qtokenName
