{-# LANGUAGE BlockArguments #-}

module Coop.Plutus (
  mkSofMp,
  mkSofV,
  pmustValidateAfter,
) where

import Control.Monad.Fail (MonadFail (fail))
import Coop.Plutus.Aux (pboolC, pfindDatum, pfindMap, pflattenValue, phasCurrency, pmaybeDataC, pownCurrencySymbol, ptryFromData, punit)
import Coop.Plutus.Types (PSofDatum, PSofMpParams, PSofVParams)
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

{- | sofV validates sof garbage collection.
- mustBeSignedBy (sofUtxo.submitter)
- mustValidateAfter sofUtxo.gcAfter
- optional mustSpendScriptOutput sofUtxo
- mustMint (sofMp publisher -1)
-}
mkSofV :: ClosedTerm (PAsData PSofVParams :--> PValidator)
mkSofV = phoistAcyclic $
  plam $ \params sofDatum _ ctx -> unTermCont do
    ownInputSof <- pletC $ pfromData (ptryFromData @PSofDatum sofDatum)

    _ <- pletC $ pmustBeSignedBy # ctx # (pfield @"sd'submittedBy" # ownInputSof)

    _ <- pletC $ pmustValidateAfter # ctx # pfromData (pfield @"sd'gcAfter" # ownInputSof)

    coopInst <- pletC $ pfield @"svp'coopInstance" # pfromData params
    sofMp <- pletC $ sofVFindSofMp # ctx # coopInst
    _ <-
      pletC $
        pmustMint # ctx
          # sofMp
          # pcon (PTokenName $ pto (pfromData (pfield @"sd'publishedBy" # ownInputSof)))
          # (pnegate # 1)

    pure $ popaque $ pconstant ()

sofVFindSofMp :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PCurrencySymbol)
sofVFindSofMp = phoistAcyclic $
  plam $ \ctx coopInst -> unTermCont do
    ptraceC "sofVFindSofMp"
    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["mint"] (getField @"txInfo" ctx')
    maySofMp <-
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
      (fail "sofVFindSofMp: couldn't deduce SofMp")
      ( \sofMp -> do
          ptraceC "sofVFindSofMp: found SofMp"
          pure sofMp
      )
      maySofMp

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

-- | Minting policy that validates creation of new sofs.
mkSofMp :: ClosedTerm (PAsData PSofMpParams :--> PMintingPolicy)
mkSofMp = phoistAcyclic $
  plam $ \params _ ctx -> unTermCont do
    _ <- pletC $ sofMpParseOutputs # pfromData params # ctx
    pure $ popaque $ pconstant ()

{- | Parse and validate transaction outputs that hold sofs.
 We rely on the level 1 ledger rule to verify that what was minted, was sent out.
 So for each of the sof output we check:
  - does it hold own currency symbol?
    - no -> skip processing of the current output (we don't err and so enable richer transactions)
    - yes
      - TODO: does it send the output to sofValidator? yes -> continue; no -> err
      - is the transaction signed by the publisher specified in the SofDatum? yes -> continue; no -> err
      - is there a single token of own currency symbol and publisher as the token name? yes -> continue; no -> err
 We don't enforce rewards at the minting policy, this is left open for the publisher to include and the submitter to accept.
 TODO: Switch to using Plutus V2
-}
sofMpParseOutputs :: Term s (PSofMpParams :--> PScriptContext :--> PBuiltinList PBool)
sofMpParseOutputs = phoistAcyclic $
  plam $ \params ctx -> unTermCont $ do
    ptraceC "sofMpParseOutputs"
    ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
    txInfo <- pletFieldsC @'["datums", "signatories", "outputs"] (getField @"txInfo" ctx')
    ownCs <- pletC $ pownCurrencySymbol # getField @"purpose" ctx'
    findDatum' <- pletC $ pfindDatum # getField @"datums" txInfo
    sigs <- pletC $ getField @"signatories" txInfo
    txOuts <- pletC $ getField @"outputs" txInfo
    sofMpParseOutput' <- pletC $ sofMpParseOutput # params # ownCs # sigs # findDatum'
    pure $ pmap # sofMpParseOutput' # txOuts

sofMpParseOutput :: Term s (PSofMpParams :--> PCurrencySymbol :--> PBuiltinList (PAsData PPubKeyHash) :--> (PDatumHash :--> PMaybeData PDatum) :--> PTxOut :--> PBool)
sofMpParseOutput = phoistAcyclic $
  plam $ \params ownCs sigs findDatum txOut -> unTermCont do
    ptraceC "sofMpParseOutput"
    txOut' <- pletFieldsC @'["value"] txOut
    outVal <- pletC $ pnormalize # getField @"value" txOut'

    hasOwnCs <- pletC $ phasCurrency # ownCs # outVal
    pboolC
      ( do
          ptraceC "sofMpParseOutput: own token not in the output, skipping"
          pure $ pcon PTrue
      )
      ( do
          ptraceC "sofMpParseOutput: found own token in the output, continuing"
          pure $ sofMpParseOutputWithSof # params # ownCs # sigs # findDatum # txOut
      )
      hasOwnCs

sofMpParseOutputWithSof :: Term s (PSofMpParams :--> PCurrencySymbol :--> PBuiltinList (PAsData PPubKeyHash) :--> (PDatumHash :--> PMaybeData PDatum) :--> PTxOut :--> PBool)
sofMpParseOutputWithSof = phoistAcyclic $
  plam $ \params ownCs sigs findDatum txOut -> unTermCont do
    ptraceC "sofMpParseOutputWithSof"
    txOut' <- pletFieldsC @'["value", "address", "datumHash"] txOut
    outVal <- pletC $ pnormalize # getField @"value" txOut'
    outAddr <- pletC $ getField @"address" txOut'

    -- TODO: Migrate to inline datums
    outDatumHash <-
      pmaybeDataC
        (fail "sofMpParseOutputWithSof: no datum present in the output")
        pure
        (getField @"datumHash" txOut')
    datum <-
      pmaybeDataC
        (fail "sofMpParseOutputWithSof: no datum with a given hash present in the transaction datums")
        pure
        (findDatum # outDatumHash)

    resDat <- pletC $ pfromData (ptryFromData @PSofDatum (pto datum))
    publishedBy <- pletC $ pfield @"sd'publishedBy" # resDat
    let publishedByBytes = pto publishedBy
    publishedByTokenName <- pletC $ pcon $ PTokenName publishedByBytes
    quantity <- pletC $ pvalueOf # outVal # ownCs # publishedByTokenName

    hasSinglePublisherToken <-
      pboolC
        (fail "sofMpParseOutputWithSof: invalid sof minting asset")
        ( do
            ptraceC "sofMpParseOutputWithSof: valid sof minting asset"
            pure $ pcon PTrue
        )
        (quantity #== 1)

    publisherIsSignatory <-
      pboolC
        (fail "sofMpParseOutputWithSof: publisher didn't sign the transaction")
        ( do
            ptraceC "sofMpParseOutputWithSof: publisher signed the transaction"
            pure $ pcon PTrue
        )
        (pelem # pdata publishedBy # sigs)

    sentToSofV <-
      pboolC
        (fail "sofMpParseOutputWithSof: output not sent to sofValidator")
        ( do
            ptraceC "sofMpParseOutputWithSof: output sent to sofValidator"
            pure $ pcon PTrue
        )
        (outAddr #== pfield @"smp'sofVAddress" # params)

    pboolC
      (fail "sofMpParseOutputWithSof: failed")
      (pure $ pcon PTrue)
      (hasSinglePublisherToken #&& publisherIsSignatory #&& sentToSofV)
