{-# LANGUAGE BlockArguments #-}

module Coop.Plutus (
  mkFsMp,
  mkFsV,
) where

import Control.Monad.Fail (MonadFail (fail))
import Coop.Plutus.Aux (pboolC, pdatumFromTxOut, pdjust, pdnothing, pfindMap, pflattenValue, phasCurrency, pmaybeDataC, pmustBeSignedBy, pmustMint, pmustValidateAfter, pownCurrencySymbol, ptryFromData, punit)
import Coop.Plutus.Types (PCertDatum, PFsDatum, PFsMpParams, PFsVParams)
import Plutarch (popaque, pto)
import Plutarch.Api.V1 (PCurrencySymbol, PMaybeData (PDJust, PDNothing), PMintingPolicy, PScriptContext, PTxInInfo, PTxOut, PValidator, ptuple)
import Plutarch.Api.V1.Value (PTokenName (PTokenName), pnormalize, pvalueOf)
import Plutarch.Bool (pif)
import Plutarch.Extra.Interval (pcontains)
import Plutarch.Extra.TermCont (pletC, pletFieldsC, ptraceC)
import Plutarch.Num (PNum (pnegate))
import Plutarch.Prelude (ClosedTerm, PAsData, PBuiltinList, PByteString, PEq ((#==)), PListLike (pcons, pnil), PUnit, Term, getField, pcon, pconstant, pdata, pdcons, pdnil, pfield, pfoldl, pfromData, phoistAcyclic, plam, plet, (#), type (:-->))
import Plutarch.TermCont (unTermCont)
import Prelude (Applicative (pure), ($))

{- | fsV validates fs garbage collection.
- mustBeSignedBy (fsUtxo.submitter)
- mustValidateAfter fsUtxo.gcAfter
- optional mustSpendScriptOutput fsUtxo
- mustMint (fsMp fsId -1)
-}
mkFsV :: ClosedTerm (PAsData PFsVParams :--> PValidator)
mkFsV = phoistAcyclic $
  plam $ \params fsDatum _ ctx -> unTermCont do
    fsDatum' <- pletC $ pfromData (ptryFromData @PFsDatum fsDatum)

    _ <- pletC $ pmustBeSignedBy # ctx # (pfield @"fd'submitter" # fsDatum')

    _ <- pletC $ pmustValidateAfter # ctx # pfromData (pfield @"fd'gcAfter" # fsDatum')

    coopInst <- pletC $ pfield @"fvp'coopInstance" # pfromData params
    fsMp <- pletC $ fsVFindFsMp # ctx # coopInst
    _ <-
      pletC $
        pmustMint # ctx
          # fsMp
          # pcon (PTokenName $ pfield @"fd'id" # fsDatum')
          # (pnegate # 1)

    pure $ popaque $ pconstant ()

fsVFindFsMp :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PCurrencySymbol)
fsVFindFsMp = phoistAcyclic $
  plam $ \ctx coopInst -> unTermCont do
    ptraceC "fsVFindFsMp"
    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["mint"] (getField @"txInfo" ctx')
    mayFsMp <-
      pletC $
        pfindMap
          # plam
            ( \asset ->
                pif
                  (pto (pfromData (pfield @"tokenName" # asset)) #== pto coopInst)
                  (pcon $ PDNothing pdnil)
                  (pcon $ PDJust $ pdcons # (pfield @"currencySymbol" # asset) # pdnil)
            )
          # (pflattenValue # getField @"mint" txInfo)
    pmaybeDataC
      (fail "fsVFindFsMp: couldn't deduce FsMp")
      ( \fsMp -> do
          ptraceC "fsVFindFsMp: found FsMp"
          pure fsMp
      )
      mayFsMp

-- | Minting policy that validates creation of new fss.
mkFsMp :: ClosedTerm (PAsData PFsMpParams :--> PMintingPolicy)
mkFsMp = phoistAcyclic $
  plam $ \params _ ctx -> unTermCont do
    validCerts <- pletC $ fsMpParseRefs # pfromData params # ctx
    validAuthInputs <- pletC $ fsMpParseInputs # pfromData params # ctx # validCerts
    _ <- pletC $ fsMpParseOutputs # pfromData params # ctx # validAuthInputs
    pure $ popaque $ pconstant ()

fsMpParseInputs ::
  Term
    s
    ( PFsMpParams
        :--> PScriptContext
        :--> PBuiltinList PCertDatum
        :--> PBuiltinList PTxInInfo
    )
fsMpParseInputs = phoistAcyclic $
  plam $ \params ctx certs -> unTermCont $ do
    ptraceC "fsMpParseInputs"
    txInfo <- pletFieldsC @'["inputs"] (pfield @"txInfo" # ctx)
    txInputs <- pletC $ getField @"inputs" txInfo
    pure $ pfoldl # (fsMpParseInput # params # ctx # certs) # pnil # txInputs

fsMpParseInput ::
  Term
    s
    ( PFsMpParams
        :--> PScriptContext
        :--> PBuiltinList PCertDatum
        :--> PBuiltinList PTxInInfo
        :--> PTxInInfo
        :--> PBuiltinList PTxInInfo
    )
fsMpParseInput = phoistAcyclic $
  plam $ \params ctx certs acc txIn -> unTermCont do
    ptraceC "fsMpParseInput"
    txInOut <- pletC $ pfield @"resolved" # txIn
    txInOutVal <- pletC $ pnormalize # (pfield @"value" # txInOut)

    authTokenCs <- pletC $ pfield @"fmp'authTokenCs" # params
    hasAuthCs <- pletC $ phasCurrency # authTokenCs # txInOutVal
    pboolC
      ( do
          ptraceC "fsMpParseInput: auth token not in the input, skipping"
          pure acc
      )
      ( do
          ptraceC "fsMpParseInput: auth token in the output, continuing"
          pure $ fsMpParseInputWithAuth # ctx # authTokenCs # certs # acc # txIn
      )
      hasAuthCs

{- | Handles a transaction input that holds an $AUTH token.

- check that there's exactly one $AUTH token that's associated with a valid Certificate
- check that the $AUTH token is burned

The TxInInfo is emitted only if it's been validated and burned.
-}
fsMpParseInputWithAuth ::
  Term
    s
    ( PScriptContext
        :--> PCurrencySymbol
        :--> PBuiltinList PCertDatum
        :--> PBuiltinList PTxInInfo
        :--> PTxInInfo
        :--> PBuiltinList PTxInInfo
    )
fsMpParseInputWithAuth = phoistAcyclic $
  plam $ \ctx authTokenCs certs acc txIn -> unTermCont do
    ptraceC "fsMpParseInputWithAuth"
    txInOut <- pletC $ pfield @"resolved" # txIn
    txInOutVal <- pletC $ pnormalize # (pfield @"value" # txInOut)

    mayAuthTn <-
      pletC $
        pfindMap
          # plam
            ( \certDat ->
                plet
                  (pcon (PTokenName $ pfield @"cert'id" # certDat))
                  ( \authTn ->
                      pif
                        (pvalueOf # txInOutVal # authTokenCs # authTn #== 1)
                        pdnothing
                        (pdjust authTn)
                  )
            )
          # certs

    pmaybeDataC
      ( do
          ptraceC "fsMpParseInputWithAuth: couldn't validate the auth token, skipping"
          pure acc
      )
      ( \authTn -> do
          ptraceC "fsMpParseInputWithAuth: auth token validated, skipping"
          _ <- pletC $ pmustMint # ctx # authTokenCs # authTn # (pnegate # 1)
          pure $ pcons # txIn # acc
      )
      mayAuthTn

fsMpParseRefs ::
  Term
    s
    ( PFsMpParams
        :--> PScriptContext
        :--> PBuiltinList PCertDatum
    )
fsMpParseRefs = phoistAcyclic $
  plam $ \params ctx -> unTermCont $ do
    -- TODO: Migrate to reference inputs
    ptraceC "fsMpParseRefs"
    txInfo <- pletFieldsC @'["inputs"] (pfield @"txInfo" # ctx)
    txInputs <- pletC $ getField @"inputs" txInfo
    pure $ pfoldl # (fsMpParseRef # params # ctx) # pnil # txInputs

fsMpParseRef ::
  Term
    s
    ( PFsMpParams
        :--> PScriptContext
        :--> PBuiltinList PCertDatum
        :--> PTxInInfo
        :--> PBuiltinList PCertDatum
    )
fsMpParseRef = phoistAcyclic $
  plam $ \params ctx acc txIn -> unTermCont do
    ptraceC "fsMpParseRef"
    txInOut <- pletC $ pfield @"resolved" # txIn
    txInOutVal <- pletC $ pnormalize # (pfield @"value" # txInOut)

    pboolC
      ( do
          ptraceC "fsMpParseRef: cert token not in the input...skipping"
          pure acc
      )
      ( do
          ptraceC "fsMpParseRef: cert token in the input...continuing"
          pure $ fsMpParseRefWithCert # params # ctx # acc # txIn
      )
      (phasCurrency # (pfield @"fmp'certTokenCs" # params) # txInOutVal)

{- | Handles a transaction input that holds an $CERT token.

- check that the transaction's validator range is contained withing the Certificate's validity range

The CertDatum is emitted only if it's been validated.
-}
fsMpParseRefWithCert ::
  Term
    s
    ( PFsMpParams
        :--> PScriptContext
        :--> PBuiltinList PCertDatum
        :--> PTxInInfo
        :--> PBuiltinList PCertDatum
    )
fsMpParseRefWithCert = phoistAcyclic $
  plam $ \_ ctx acc txIn -> unTermCont do
    ptraceC "fsMpParseRefWithCert"
    txInOut <- pletC $ pfield @"resolved" # txIn

    certDat <- pletC $ (pdatumFromTxOut @PCertDatum) # ctx # txInOut
    certValidity <- pletC $ pfield @"cert'validity" # certDat
    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["validRange"] (getField @"txInfo" ctx')
    txValidRange <- pletC $ pfromData $ getField @"validRange" txInfo
    pboolC
      ( do
          ptraceC "fsMpParseRefWithCert: cert is invalid"
          pure acc
      )
      ( do
          ptraceC "fsMpParseRefWithCert: cert is valid"
          pure $ pcons # certDat # acc
      )
      (pcontains # certValidity # txValidRange)

{- | Parse and validate transaction outputs that hold Fact Statements.
 TODO: Switch to using Plutus V2
-}
fsMpParseOutputs :: Term s (PFsMpParams :--> PScriptContext :--> PBuiltinList PTxInInfo :--> PUnit)
fsMpParseOutputs = phoistAcyclic $
  plam $ \params ctx validAuthInputs -> unTermCont $ do
    ptraceC "fsMpParseOutputs"
    ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
    txInfo <- pletFieldsC @'["datums", "outputs"] (getField @"txInfo" ctx')
    ownCs <- pletC $ pownCurrencySymbol # getField @"purpose" ctx'
    txOuts <- pletC $ getField @"outputs" txInfo
    fsMpParseOutput' <- pletC $ fsMpParseOutput # params # ctx # ownCs
    _ <- pletC $ pfoldl # fsMpParseOutput' # validAuthInputs # txOuts
    pure punit

fsMpParseOutput ::
  Term
    s
    ( PFsMpParams
        :--> PScriptContext
        :--> PCurrencySymbol
        :--> PBuiltinList PTxInInfo
        :--> PTxOut
        :--> PBuiltinList PTxInInfo
    )
fsMpParseOutput = phoistAcyclic $
  plam $ \params ctx ownCs validAuthInputs txOut -> unTermCont do
    ptraceC "fsMpParseOutput"
    txOut' <- pletFieldsC @'["value"] txOut
    outVal <- pletC $ pnormalize # getField @"value" txOut'

    hasOwnCs <- pletC $ phasCurrency # ownCs # outVal
    pboolC
      ( do
          ptraceC "fsMpParseOutput: own token not in the output, skipping"
          pure validAuthInputs
      )
      ( do
          ptraceC "fsMpParseOutput: found own token in the output, continuing"
          pure $ fsMpParseOutputWithFs # params # ctx # ownCs # validAuthInputs # txOut
      )
      hasOwnCs

{- | Handles a transaction output that holds an $FS token.

- check that 1 $FS is minted and associated with a valid $AUTH
- check that 1 $FS is sent to FsV

The CertDatum is emitted only if it's been validated.
-}
fsMpParseOutputWithFs ::
  Term
    s
    ( PFsMpParams
        :--> PScriptContext
        :--> PCurrencySymbol
        :--> PBuiltinList PTxInInfo
        :--> PTxOut
        :--> PBuiltinList PTxInInfo
    )
fsMpParseOutputWithFs = phoistAcyclic $
  plam $ \params ctx ownCs validAuthInputs txOut -> unTermCont do
    ptraceC "fsMpParseOutputWithFs"
    txOut' <- pletFieldsC @'["value", "address", "datumHash"] txOut
    outVal <- pletC $ pnormalize # txOut'.value
    outAddr <- pletC $ txOut'.address

    foundAndRest <-
      pletC $
        pfoldl
          # plam
            ( \foundAndRest authInput ->
                plet
                  (pcon (PTokenName $ ptxIdFromTxInInfo # authInput))
                  ( \fsTn ->
                      pif
                        (pvalueOf # outVal # ownCs # fsTn #== 1)
                        (ptuple # pdata pdnothing # pdata (pcons # authInput # (pfield @"_1" # foundAndRest)))
                        (ptuple # pdata (pdjust fsTn) # pdata (pfield @"_1" # foundAndRest))
                  )
            )
          # ( ptuple
                # pdata (pdnothing :: Term _ (PMaybeData PTokenName))
                # pdata (pnil :: Term _ (PBuiltinList PTxInInfo))
            )
          # validAuthInputs

    mayFsTn <- pletC $ pfield @"_0" # foundAndRest
    restAuthInputs <- pletC $ pfield @"_1" # foundAndRest

    pboolC
      ( do
          fail "fsMpParseOutputWithFs: output not sent to fsV"
          pure punit
      )
      ( do
          ptraceC "fsMpParseOutputWithFs: output sent to fsV"
          pure punit
      )
      (outAddr #== pfield @"fmp'fsVAddress" # params)

    pmaybeDataC
      ( do
          fail "fsMpParseOutputWithFs: Correct $FS not found"
          pure validAuthInputs
      )
      ( \_ -> do
          ptraceC "fsMpParseOutputWithFs: Correct $FS found"
          pure restAuthInputs
      )
      mayFsTn

ptxIdFromTxInInfo :: Term s (PTxInInfo :--> PByteString)
ptxIdFromTxInInfo = phoistAcyclic $ plam \inInfo -> pfield @"_0" # (pfield @"id" # (pfield @"outRef" # inInfo))
