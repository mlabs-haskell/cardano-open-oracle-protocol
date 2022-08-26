{-# LANGUAGE BlockArguments #-}

module Coop.Plutus (
  mkFsMp,
  mkFsV,
) where

import Control.Monad.Fail (MonadFail (fail))
import Coop.Plutus.Aux (pboolC, pdatumFromTxOut, pdjust, pdnothing, pfindMap, pgetTokensByCurrency, phasCurrency, pmaybeDataC, pmustBeSignedBy, pmustMint, pmustValidateAfter, pownCurrencySymbol, ptryFromData, punit)
import Coop.Plutus.Types (PAuthParams, PCertDatum, PFsDatum, PFsMpParams, PFsVParams)
import Plutarch (popaque)
import Plutarch.Api.V1 (AmountGuarantees (NonZero), KeyGuarantees (Sorted), PCurrencySymbol, PMaybeData, PMintingPolicy, PScriptContext, PTxInInfo, PTxOut, PValidator, PValue, ptuple)
import Plutarch.Api.V1.AssocMap (psingleton)
import Plutarch.Api.V1.Value (PTokenName (PTokenName), pnormalize)
import Plutarch.Bool (pif)
import Plutarch.Extra.Interval (pcontains)
import Plutarch.Extra.TermCont (pletC, pletFieldsC, ptraceC)
import Plutarch.Num (PNum (pnegate))
import Plutarch.Prelude (ClosedTerm, PAsData, PBuiltinList, PEq ((#==)), PListLike (pcons, pnil), PUnit, Term, getField, pcon, pconsBS, pconstant, pdata, pfield, pfoldl, pfromData, phoistAcyclic, plam, plet, psha3_256, (#), type (:-->))
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
  plam $ \_ fsDatum _ ctx -> unTermCont do
    ptraceC "@FsV"

    fsDatum' <- pletFieldsC @'["fd'submitter", "fd'gcAfter", "fd'fsCs", "fd'id"] $ pfromData (ptryFromData @PFsDatum fsDatum)

    _ <- pletC $ pmustBeSignedBy # ctx # fsDatum'.fd'submitter
    ptraceC "@FsV: Submitter signed"

    _ <- pletC $ pmustValidateAfter # ctx # fsDatum'.fd'gcAfter
    ptraceC "@FsV: Can collect"

    fsCs <- pletC $ fsDatum'.fd'fsCs
    fsTn <- pletC $ pcon (PTokenName $ fsDatum'.fd'id)
    _ <- pletC $ pmustMint # ctx # fsCs # fsTn # (pnegate # 1)
    ptraceC "@FsV: $FS burned"

    pure $ popaque $ pconstant ()

-- TODO: Add FS_MINT and FS_BURN Redeemers

-- | Minting policy that validates creation of new fss.
mkFsMp :: ClosedTerm (PAsData PFsMpParams :--> PMintingPolicy)
mkFsMp = phoistAcyclic $
  plam $ \params _ ctx -> unTermCont do
    ptraceC "FsMp"

    validAuthInputs <- pletC $ caValidateAuthInputs # (pfield @"fmp'authParams" # params) # ctx
    ptraceC "FsMp: Validated authentication inputs"

    _ <- pletC $ fsMpParseOutputs # pfromData params # ctx # validAuthInputs
    ptraceC "FsMp: Validated Fact Statement outputs"

    pure $ popaque $ pconstant ()

{- | Parse and validate transaction outputs that hold Fact Statements.
 TODO: Switch to using Plutus V2
-}
fsMpParseOutputs :: Term s (PFsMpParams :--> PScriptContext :--> PBuiltinList PTxInInfo :--> PUnit)
fsMpParseOutputs = phoistAcyclic $
  plam $ \params ctx validAuthInputs -> unTermCont $ do
    ptraceC "fsMpParseOutputs"
    ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
    txInfo <- pletFieldsC @'["outputs"] (ctx'.txInfo)
    ownCs <- pletC $ pownCurrencySymbol # ctx'.purpose
    txOuts <- pletC $ txInfo.outputs
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
    outVal <- pletC $ pnormalize # txOut'.value

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

The TxInInfo is emitted only if it's been validated. However, if any check fails the failure is raised.
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
    txOut' <- pletFieldsC @'["value", "address"] txOut
    outVal <- pletC $ pnormalize # txOut'.value
    outAddr <- pletC $ txOut'.address

    pboolC
      (fail "fsMpParseOutputWithFs: output not sent to fsV")
      (ptraceC "fsMpParseOutputWithFs: output sent to fsV")
      (outAddr #== (pfield @"fmp'fsVAddress" # params))

    ownTokens <- pletC $ pgetTokensByCurrency # ownCs # outVal
    foundAndRest <-
      pletC $
        pfoldl
          # plam
            ( \foundAndRest authInput ->
                plet
                  (ptokenNameFromTxInInfo # authInput)
                  ( \fsTn ->
                      pif
                        (ownTokens #== (psingleton # fsTn # 1)) -- NOTE: Only outputs a single $FS token!
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

    pmaybeDataC
      (fail "fsMpParseOutputWithFs: invalid $FS not found")
      ( \fsTn -> do
          ptraceC "fsMpParseOutputWithFs: $FS validated"
          _ <- pletC $ pmustMint # ctx # ownCs # fsTn # 1
          ptraceC "fsMpParseOutputWithFs: $FS minted"
      )
      mayFsTn

    pure restAuthInputs

{- | ptokenNameFromTxInInfo creates a unique TokenName from the given transaction input

TokenName $ sha3_256 (refId `concat` num)
-}
ptokenNameFromTxInInfo :: Term s (PTxInInfo :--> PTokenName)
ptokenNameFromTxInInfo = phoistAcyclic $
  plam $ \inInfo -> unTermCont do
    txId <- pletC $ pfield @"_0" # (pfield @"id" # (pfield @"outRef" # inInfo))
    txIdx <- pletC $ pfield @"idx" # (pfield @"outRef" # inInfo)
    pure $ pcon $ PTokenName $ psha3_256 # (pconsBS # txIdx # txId)

-- | CoopMp authentication
caValidateAuthInputs ::
  Term
    s
    ( PAuthParams
        :--> PScriptContext
        :--> PBuiltinList PTxInInfo
    )
caValidateAuthInputs = phoistAcyclic $
  plam $ \params ctx -> unTermCont $ do
    validCerts <- pletC $ caParseRefs # params # ctx
    pure $ caParseInputs # params # ctx # validCerts

caParseInputs ::
  Term
    s
    ( PAuthParams
        :--> PScriptContext
        :--> PBuiltinList PCertDatum
        :--> PBuiltinList PTxInInfo
    )
caParseInputs = phoistAcyclic $
  plam $ \params ctx certs -> unTermCont $ do
    ptraceC "caParseInputs"
    txInfo <- pletFieldsC @'["inputs"] (pfield @"txInfo" # ctx)
    txInputs <- pletC $ getField @"inputs" txInfo
    pure $ pfoldl # (caParseInput # params # ctx # certs) # pnil # txInputs

caParseInput ::
  Term
    s
    ( PAuthParams
        :--> PScriptContext
        :--> PBuiltinList PCertDatum
        :--> PBuiltinList PTxInInfo
        :--> PTxInInfo
        :--> PBuiltinList PTxInInfo
    )
caParseInput = phoistAcyclic $
  plam $ \params ctx certs acc txIn -> unTermCont do
    ptraceC "caParseInput"
    txInOut <- pletC $ pfield @"resolved" # txIn
    txInOutVal <- pletC $ pnormalize # (pfield @"value" # txInOut)

    authTokenCs <- pletC $ pfield @"ap'authTokenCs" # params
    hasAuthCs <- pletC $ phasCurrency # authTokenCs # txInOutVal
    pboolC
      ( do
          ptraceC "caParseInput: auth token not in the input, skipping"
          pure acc
      )
      ( do
          ptraceC "caParseInput: auth token in the output, continuing"
          pure $ caParseInputWithAuth # ctx # authTokenCs # certs # acc # txIn
      )
      hasAuthCs

{- | Handles a transaction input that holds an $AUTH token.

- check that there's exactly one $AUTH token that's associated with a valid Certificate
- check that the $AUTH token is burned

The TxInInfo is emitted only if it's been validated and burned.
The function fails hard if any of the checks fails.
-}
caParseInputWithAuth ::
  Term
    s
    ( PScriptContext
        :--> PCurrencySymbol
        :--> PBuiltinList PCertDatum
        :--> PBuiltinList PTxInInfo
        :--> PTxInInfo
        :--> PBuiltinList PTxInInfo
    )
caParseInputWithAuth = phoistAcyclic $
  plam $ \ctx authTokenCs certs acc txIn -> unTermCont do
    ptraceC "caParseInputWithAuth"
    txInOut <- pletC $ pfield @"resolved" # txIn
    txInVal <- pletC $ pnormalize # (pfield @"value" # txInOut)

    authTokens <- pletC $ pgetTokensByCurrency # authTokenCs # txInVal

    mayAuthTn <-
      pletC $
        pfindMap
          # plam
            ( \certDat ->
                plet
                  (pcon (PTokenName $ pfield @"cert'id" # certDat))
                  ( \authTn ->
                      pif
                        (authTokens #== (psingleton # authTn # 1)) -- NOTE: Only contains a single $AUTH token!
                        pdnothing
                        (pdjust authTn)
                  )
            )
          # certs

    pmaybeDataC
      (fail "caParseInputWithAuth: couldn't validate the auth token, skipping")
      ( \authTn -> do
          ptraceC "caParseInputWithAuth: auth token validated, continuing"
          _ <- pletC $ pmustMint # ctx # authTokenCs # authTn # (pnegate # 1)
          ptraceC "caParseInputWithAuth: auth token burned, continuing"
      )
      mayAuthTn

    pure $ pcons # txIn # acc

caParseRefs ::
  Term
    s
    ( PAuthParams
        :--> PScriptContext
        :--> PBuiltinList PCertDatum
    )
caParseRefs = phoistAcyclic $
  plam $ \params ctx -> unTermCont $ do
    -- TODO: Migrate to reference inputs
    ptraceC "caParseRefs"
    txInfo <- pletFieldsC @'["inputs"] (pfield @"txInfo" # ctx)
    txInputs <- pletC $ getField @"inputs" txInfo
    pure $ pfoldl # (caParseRef # params # ctx) # pnil # txInputs

caParseRef ::
  Term
    s
    ( PAuthParams
        :--> PScriptContext
        :--> PBuiltinList PCertDatum
        :--> PTxInInfo
        :--> PBuiltinList PCertDatum
    )
caParseRef = phoistAcyclic $
  plam $ \params ctx acc txIn -> unTermCont do
    ptraceC "caParseRef"
    txInOut <- pletC $ pfield @"resolved" # txIn
    txInVal <- pletC $ pnormalize # (pfield @"value" # txInOut)
    certTokenCs <- pletC $ pfield @"ap'certTokenCs" # params

    pboolC
      ( do
          ptraceC "caParseRef: cert token not in the input, skipping"
          pure acc
      )
      ( do
          ptraceC "caParseRef: cert token in the input, continuing"
          pure $ caParseRefWithCert # ctx # certTokenCs # txInVal # acc # txIn
      )
      (phasCurrency # certTokenCs # txInVal)

{- | Handles a transaction input that holds an $CERT token.

- check that the transaction's validator range is contained withing the Certificate's validity range
- check that the Certificate ref input has 1 $CERT token with appropriate ID (TokenName)

The CertDatum is emitted only if it's been validated.
The function fails hard if any of the checks fails.
-}
caParseRefWithCert ::
  Term
    s
    ( PScriptContext
        :--> PCurrencySymbol
        :--> PValue 'Sorted 'NonZero
        :--> PBuiltinList PCertDatum
        :--> PTxInInfo
        :--> PBuiltinList PCertDatum
    )
caParseRefWithCert = phoistAcyclic $
  plam $ \ctx certTokenCs txInVal acc txIn -> unTermCont do
    ptraceC "caParseRefWithCert"
    txInOut <- pletC $ pfield @"resolved" # txIn

    certDat <- pletC $ (pdatumFromTxOut @PCertDatum) # ctx # txInOut
    certValidity <- pletC $ pfield @"cert'validity" # certDat
    certId <- pletC $ pfield @"cert'id" # certDat

    certTokens <- pletC $ pgetTokensByCurrency # certTokenCs # txInVal
    pboolC
      (fail "caParseRefWithCert: cert mismatched id")
      (ptraceC "caParseRefWithCert: cert has a matching id")
      (certTokens #== (psingleton # pcon (PTokenName certId) # 1))

    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["validRange"] (getField @"txInfo" ctx')
    txValidRange <- pletC $ pfromData $ getField @"validRange" txInfo
    pboolC
      (fail "caParseRefWithCert: cert is invalid")
      (ptraceC "caParseRefWithCert: cert is valid")
      (pcontains # certValidity # txValidRange)

    pure $ pcons # certDat # acc
