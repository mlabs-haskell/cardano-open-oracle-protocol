{-# LANGUAGE BlockArguments #-}

module Coop.Plutus (
  mkFsMp,
  mkFsV,
) where

import Control.Monad.Fail (MonadFail (fail))
import Coop.Plutus.Aux (pboolC, pfindDatum, pfindMap, pflattenValue, phasCurrency, pmaybeDataC, pownCurrencySymbol, ptryFromData, punit)
import Coop.Plutus.Types (PFsDatum, PFsMpParams, PFsVParams)
import Plutarch (popaque, pto)
import Plutarch.Api.V1 (PCurrencySymbol, PDatum, PDatumHash, PMaybeData (PDJust, PDNothing), PMintingPolicy, PPOSIXTime, PPubKeyHash, PScriptContext, PTxOut, PValidator)
import Plutarch.Api.V1.Value (PTokenName (PTokenName), pnormalize, pvalueOf)
import Plutarch.Bool (PBool (PTrue), pif)
import Plutarch.Extra.Interval (pbefore)
import Plutarch.Extra.TermCont (pletC, pletFieldsC, ptraceC)
import Plutarch.Num (PNum (pnegate))
import Plutarch.Prelude (ClosedTerm, PAsData, PBuiltinList, PEq ((#==)), PInteger, PUnit, Term, getField, pcon, pconstant, pdata, pdcons, pdnil, pelem, pfield, pfromData, phoistAcyclic, plam, pmap, (#), (#&&), type (:-->))
import Plutarch.TermCont (unTermCont)
import Prelude (Applicative (pure), ($))

{- | fsV validates fs garbage collection.
- mustBeSignedBy (fsUtxo.submitter)
- mustValidateAfter fsUtxo.gcAfter
- optional mustSpendScriptOutput fsUtxo
- mustMint (fsMp publisher -1)
-}
mkFsV :: ClosedTerm (PAsData PFsVParams :--> PValidator)
mkFsV = phoistAcyclic $
  plam $ \params fsDatum _ ctx -> unTermCont do
    ownInputFs <- pletC $ pfromData (ptryFromData @PFsDatum fsDatum)

    _ <- pletC $ pmustBeSignedBy # ctx # (pfield @"fd'submittedBy" # ownInputFs)

    _ <- pletC $ pmustValidateAfter # ctx # pfromData (pfield @"fd'gcAfter" # ownInputFs)

    coopInst <- pletC $ pfield @"fvp'coopInstance" # pfromData params
    fsMp <- pletC $ fsVFindFsMp # ctx # coopInst
    _ <-
      pletC $
        pmustMint # ctx
          # fsMp
          # pcon (PTokenName $ pto (pfromData (pfield @"fd'publishedBy" # ownInputFs)))
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

pmustMint :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PTokenName :--> PInteger :--> PUnit)
pmustMint = phoistAcyclic $
  plam $ \ctx cs tn q -> unTermCont do
    ptraceC "mustMint"
    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["mint"] (getField @"txInfo" ctx')
    mint <- pletC $ getField @"mint" txInfo
    pboolC
      (fail "pmustMint: didn't mint the specified quantity")
      ( do
          ptraceC "pmustMint: minted specified quantity"
          pure punit
      )
      (pvalueOf # mint # cs # tn #== q)

pmustValidateAfter :: ClosedTerm (PScriptContext :--> PPOSIXTime :--> PUnit)
pmustValidateAfter = phoistAcyclic $
  plam $ \ctx after -> unTermCont do
    ptraceC "mustValidateAfter"
    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["validRange"] (getField @"txInfo" ctx')

    txValidRange <- pletC $ pfromData $ getField @"validRange" txInfo
    pboolC
      (fail "pmustValidateAfter: transaction validation range is not after 'after'")
      ( do
          ptraceC "pmustValidateAfter: transaction validation range is after 'after'"
          pure punit
      )
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
      ( do
          ptraceC "mustBeSignedBy: pkh signed the transaction"
          pure punit
      )
      (pelem # pdata pkh # sigs)

-- | Minting policy that validates creation of new fss.
mkFsMp :: ClosedTerm (PAsData PFsMpParams :--> PMintingPolicy)
mkFsMp = phoistAcyclic $
  plam $ \params _ ctx -> unTermCont do
    _ <- pletC $ fsMpParseOutputs # pfromData params # ctx
    pure $ popaque $ pconstant ()

{- | Parse and validate transaction outputs that hold Fact Statements.
 We rely on the level 1 ledger rule to verify that what was minted, was sent out.
 So for each of the fs output we check:
  - does it hold own currency symbol?
    - no -> skip processing of the current output (we don't err and so enable richer transactions)
    - yes
      - TODO: does it send the output to fsValidator? yes -> continue; no -> err
      - is the transaction signed by the publisher specified in the FsDatum? yes -> continue; no -> err
      - is there a single token of own currency symbol and publisher as the token name? yes -> continue; no -> err
 We don't enforce rewards at the minting policy, this is left open for the publisher to include and the submitter to accept.
 TODO: Switch to using Plutus V2
-}
fsMpParseOutputs :: Term s (PFsMpParams :--> PScriptContext :--> PBuiltinList PBool)
fsMpParseOutputs = phoistAcyclic $
  plam $ \params ctx -> unTermCont $ do
    ptraceC "fsMpParseOutputs"
    ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
    txInfo <- pletFieldsC @'["datums", "signatories", "outputs"] (getField @"txInfo" ctx')
    ownCs <- pletC $ pownCurrencySymbol # getField @"purpose" ctx'
    findDatum' <- pletC $ pfindDatum # getField @"datums" txInfo
    sigs <- pletC $ getField @"signatories" txInfo
    txOuts <- pletC $ getField @"outputs" txInfo
    fsMpParseOutput' <- pletC $ fsMpParseOutput # params # ownCs # sigs # findDatum'
    pure $ pmap # fsMpParseOutput' # txOuts

fsMpParseOutput :: Term s (PFsMpParams :--> PCurrencySymbol :--> PBuiltinList (PAsData PPubKeyHash) :--> (PDatumHash :--> PMaybeData PDatum) :--> PTxOut :--> PBool)
fsMpParseOutput = phoistAcyclic $
  plam $ \params ownCs sigs findDatum txOut -> unTermCont do
    ptraceC "fsMpParseOutput"
    txOut' <- pletFieldsC @'["value"] txOut
    outVal <- pletC $ pnormalize # getField @"value" txOut'

    hasOwnCs <- pletC $ phasCurrency # ownCs # outVal
    pboolC
      ( do
          ptraceC "fsMpParseOutput: own token not in the output, skipping"
          pure $ pcon PTrue
      )
      ( do
          ptraceC "fsMpParseOutput: found own token in the output, continuing"
          pure $ fsMpParseOutputWithFs # params # ownCs # sigs # findDatum # txOut
      )
      hasOwnCs

fsMpParseOutputWithFs :: Term s (PFsMpParams :--> PCurrencySymbol :--> PBuiltinList (PAsData PPubKeyHash) :--> (PDatumHash :--> PMaybeData PDatum) :--> PTxOut :--> PBool)
fsMpParseOutputWithFs = phoistAcyclic $
  plam $ \params ownCs sigs findDatum txOut -> unTermCont do
    ptraceC "fsMpParseOutputWithFs"
    txOut' <- pletFieldsC @'["value", "address", "datumHash"] txOut
    outVal <- pletC $ pnormalize # getField @"value" txOut'
    outAddr <- pletC $ getField @"address" txOut'

    -- TODO: Migrate to inline datums
    outDatumHash <-
      pmaybeDataC
        (fail "fsMpParseOutputWithFs: no datum present in the output")
        pure
        (getField @"datumHash" txOut')
    datum <-
      pmaybeDataC
        (fail "fsMpParseOutputWithFs: no datum with a given hash present in the transaction datums")
        pure
        (findDatum # outDatumHash)

    resDat <- pletC $ pfromData (ptryFromData @PFsDatum (pto datum))
    publishedBy <- pletC $ pfield @"fd'publishedBy" # resDat
    let publishedByBytes = pto publishedBy
    publishedByTokenName <- pletC $ pcon $ PTokenName publishedByBytes
    quantity <- pletC $ pvalueOf # outVal # ownCs # publishedByTokenName

    hasSinglePublisherToken <-
      pboolC
        (fail "fsMpParseOutputWithFs: invalid fs minting asset")
        ( do
            ptraceC "fsMpParseOutputWithFs: valid fs minting asset"
            pure $ pcon PTrue
        )
        (quantity #== 1)

    publisherIsSignatory <-
      pboolC
        (fail "fsMpParseOutputWithFs: publisher didn't sign the transaction")
        ( do
            ptraceC "fsMpParseOutputWithFs: publisher signed the transaction"
            pure $ pcon PTrue
        )
        (pelem # pdata publishedBy # sigs)

    sentToFsV <-
      pboolC
        (fail "fsMpParseOutputWithFs: output not sent to fsValidator")
        ( do
            ptraceC "fsMpParseOutputWithFs: output sent to fsValidator"
            pure $ pcon PTrue
        )
        (outAddr #== pfield @"fmp'fsVAddress" # params)

    pboolC
      (fail "fsMpParseOutputWithFs: failed")
      (pure $ pcon PTrue)
      (hasSinglePublisherToken #&& publisherIsSignatory #&& sentToFsV)
