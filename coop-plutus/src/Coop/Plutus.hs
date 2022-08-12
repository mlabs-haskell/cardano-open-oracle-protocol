{-# LANGUAGE BlockArguments #-}

module Coop.Plutus (
  mkSofMp,
  mkSofV,
) where

import Control.Monad.Fail (MonadFail (fail))
import Coop.Plutus.Aux (pboolC, pfindDatum, phasCurrency, pmaybeDataC, pownCurrencySymbol, ptryFromData)
import Coop.Plutus.Types (PSofDatum, PSofMpParams, PSofVParams)
import Plutarch (popaque, pto)
import Plutarch.Api.V1 (PCurrencySymbol, PDatum, PDatumHash, PMaybeData, PMintingPolicy, PPubKeyHash, PScriptContext, PTxOut, PValidator)
import Plutarch.Api.V1.Value (PTokenName (PTokenName), pnormalize, pvalueOf)
import Plutarch.Bool (PBool (PTrue))
import Plutarch.Extra.TermCont (pletC, pletFieldsC, ptraceC)
import Plutarch.Prelude (ClosedTerm, PAsData, PBuiltinList, PEq ((#==)), Term, getField, pcon, pconstant, pdata, pelem, pfield, pfromData, phoistAcyclic, plam, pmap, (#), (#&&), type (:-->))
import Plutarch.TermCont (unTermCont)
import Prelude (Applicative (pure), ($))

mkSofV :: ClosedTerm (PAsData PSofVParams :--> PValidator)
mkSofV = phoistAcyclic $ plam $ \_ _ _ _ -> popaque $ pconstant ()

-- | Minting policy that validates creation of new sofs.
mkSofMp :: ClosedTerm (PAsData PSofMpParams :--> PMintingPolicy)
mkSofMp = phoistAcyclic $
  plam $ \params _ ctx -> unTermCont do
    _ <- pletC $ parseOutputs # pfromData params # ctx
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
parseOutputs :: Term s (PSofMpParams :--> PScriptContext :--> PBuiltinList PBool)
parseOutputs = phoistAcyclic $
  plam $ \params ctx -> unTermCont $ do
    ptraceC "parseOutputs"
    ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
    txInfo <- pletFieldsC @'["datums", "signatories", "outputs"] (getField @"txInfo" ctx')
    ownCs <- pletC $ pownCurrencySymbol # getField @"purpose" ctx'
    findDatum' <- pletC $ pfindDatum # getField @"datums" txInfo
    sigs <- pletC $ getField @"signatories" txInfo
    txOuts <- pletC $ getField @"outputs" txInfo
    parseOutput' <- pletC $ parseOutput # params # ownCs # sigs # findDatum'
    pure $ pmap # parseOutput' # txOuts

parseOutput :: Term s (PSofMpParams :--> PCurrencySymbol :--> PBuiltinList (PAsData PPubKeyHash) :--> (PDatumHash :--> PMaybeData PDatum) :--> PTxOut :--> PBool)
parseOutput = phoistAcyclic $
  plam $ \params ownCs sigs findDatum txOut -> unTermCont do
    ptraceC "parseOutput"
    txOut' <- pletFieldsC @'["value"] txOut
    outVal <- pletC $ pnormalize # getField @"value" txOut'

    hasOwnCs <- pletC $ phasCurrency # ownCs # outVal
    pboolC
      ( do
          ptraceC "parseOutput: own token not in the output, skipping"
          pure $ pcon PTrue
      )
      ( do
          ptraceC "parseOutput: found own token in the output, continuing"
          pure $ parseOutputWithSof # params # ownCs # sigs # findDatum # txOut
      )
      hasOwnCs

parseOutputWithSof :: Term s (PSofMpParams :--> PCurrencySymbol :--> PBuiltinList (PAsData PPubKeyHash) :--> (PDatumHash :--> PMaybeData PDatum) :--> PTxOut :--> PBool)
parseOutputWithSof = phoistAcyclic $
  plam $ \params ownCs sigs findDatum txOut -> unTermCont do
    ptraceC "parseOutputWithSof"
    txOut' <- pletFieldsC @'["value", "address", "datumHash"] txOut
    outVal <- pletC $ pnormalize # getField @"value" txOut'
    outAddr <- pletC $ getField @"address" txOut'

    outDatumHash <-
      pmaybeDataC
        (fail "parseOutputWithSof: no datum present in the output")
        pure
        (getField @"datumHash" txOut')
    datum <-
      pmaybeDataC
        (fail "parseOutputWithSof: no datum with a given hash present in the transaction datums")
        pure
        (findDatum # outDatumHash)

    resDat <- pletC $ pfromData (ptryFromData @PSofDatum (pto datum))
    publishedBy <- pletC $ pfield @"sd'publishedBy" # resDat
    let publishedByBytes = pto publishedBy
    publishedByTokenName <- pletC $ pcon $ PTokenName publishedByBytes
    quantity <- pletC $ pvalueOf # outVal # ownCs # publishedByTokenName

    hasSinglePublisherToken <-
      pboolC
        (fail "parseOutputWithSof: invalid sof minting asset")
        ( do
            ptraceC "parseOutputWithSof: valid sof minting asset"
            pure $ pcon PTrue
        )
        (quantity #== 1)

    publisherIsSignatory <-
      pboolC
        (fail "parseOutputWithSof: publisher didn't sign the transaction")
        ( do
            ptraceC "parseOutputWithSof: publisher signed the transaction"
            pure $ pcon PTrue
        )
        (pelem # pdata publishedBy # sigs)

    sentToSofV <-
      pboolC
        (fail "parseOutputWithSof: output not sent to sofValidator")
        ( do
            ptraceC "parseOutputWithSof: output sent to sofValidator"
            pure $ pcon PTrue
        )
        (outAddr #== pfield @"smp'sofVAddress" # params)

    pboolC
      (fail "parseOutputWithSof: failed")
      (pure $ pcon PTrue)
      (hasSinglePublisherToken #&& publisherIsSignatory #&& sentToSofV)
