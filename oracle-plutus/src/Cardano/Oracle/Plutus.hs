{-# LANGUAGE BlockArguments #-}

module Cardano.Oracle.Plutus (
  resourceMintingPolicy,
  resourceValidator,
) where

import Cardano.Oracle.Plutus.Aux (pboolC, pfindDatum, phasCurrency, pmaybeDataC, pownCurrencySymbol, ptryFromData)
import Cardano.Oracle.Plutus.Types (PResourceDatum, PResourceMintingParams, PResourceValidatorParams)
import Control.Monad.Fail (MonadFail (fail))
import Plutarch (popaque, pto)
import Plutarch.Api.V1 (PCurrencySymbol, PDatum, PDatumHash, PMaybeData, PMintingPolicy, PPubKeyHash, PScriptContext, PTxOut, PValidator)
import Plutarch.Api.V1.Value (PTokenName (PTokenName), pnormalize, pvalueOf)
import Plutarch.Bool (PBool (PTrue))
import Plutarch.Extra.TermCont (pletC, pletFieldsC, ptraceC)
import Plutarch.Prelude (ClosedTerm, PAsData, PBuiltinList, PEq ((#==)), Term, getField, pcon, pconstant, pdata, pelem, pfield, pfromData, phoistAcyclic, plam, pmap, (#), (#&&), type (:-->))
import Plutarch.TermCont (unTermCont)
import Prelude (Applicative (pure), ($))

-- TODO: Parametrize by the one-shot token and resourceMintingPolicy
resourceValidator :: ClosedTerm (PAsData PResourceValidatorParams :--> PValidator)
resourceValidator = phoistAcyclic $ plam $ \_ _ _ _ -> popaque $ pconstant ()

-- | Minting policy that validates creation of new resources.
resourceMintingPolicy :: ClosedTerm (PAsData PResourceMintingParams :--> PMintingPolicy)
resourceMintingPolicy = phoistAcyclic $
  plam $ \params _ ctx -> unTermCont do
    _ <- pletC $ parseOutputs # pfromData params # ctx
    pure $ popaque $ pconstant ()

{- | Parse and validate transaction outputs that hold resources.
 We rely on the level 1 ledger rule to verify that what was minted, was sent out.
 So for each of the resource output we check:
  - does it hold own currency symbol?
    - no -> skip processing of the current output (we don't err and so enable richer transactions)
    - yes
      - TODO: does it send the output to resourceValidator? yes -> continue; no -> err
      - is the transaction signed by the publisher specified in the ResourceDatum? yes -> continue; no -> err
      - is there a single token of own currency symbol and publisher as the token name? yes -> continue; no -> err
 We don't enforce rewards at the minting policy, this is left open for the publisher to include and the submitter to accept.
 TODO: Switch to using Plutus V2
-}
parseOutputs :: Term s (PResourceMintingParams :--> PScriptContext :--> PBuiltinList PBool)
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

parseOutput :: Term s (PResourceMintingParams :--> PCurrencySymbol :--> PBuiltinList (PAsData PPubKeyHash) :--> (PDatumHash :--> PMaybeData PDatum) :--> PTxOut :--> PBool)
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
          pure $ parseOutputWithResource # params # ownCs # sigs # findDatum # txOut
      )
      hasOwnCs

parseOutputWithResource :: Term s (PResourceMintingParams :--> PCurrencySymbol :--> PBuiltinList (PAsData PPubKeyHash) :--> (PDatumHash :--> PMaybeData PDatum) :--> PTxOut :--> PBool)
parseOutputWithResource = phoistAcyclic $
  plam $ \params ownCs sigs findDatum txOut -> unTermCont do
    ptraceC "parseOutputWithResource"
    txOut' <- pletFieldsC @'["value", "address", "datumHash"] txOut
    outVal <- pletC $ pnormalize # getField @"value" txOut'
    outAddr <- pletC $ getField @"address" txOut'

    outDatumHash <-
      pmaybeDataC
        (fail "parseOutputWithResource: no datum present in the output")
        pure
        (getField @"datumHash" txOut')
    datum <-
      pmaybeDataC
        (fail "parseOutputWithResource: no datum with a given hash present in the transaction datums")
        pure
        (findDatum # outDatumHash)

    resDat <- pletC $ pfromData (ptryFromData @PResourceDatum (pto datum))
    publishedBy <- pletC $ pfield @"publishedBy" # resDat
    let publishedByBytes = pto publishedBy
    publishedByTokenName <- pletC $ pcon $ PTokenName publishedByBytes -- TODO: Test TokenName invariants (hex 32bytes)
    quantity <- pletC $ pvalueOf # outVal # ownCs # publishedByTokenName

    hasSinglePublisherToken <-
      pboolC
        (fail "parseOutputWithResource: invalid resource minting asset")
        ( do
            ptraceC "parseOutputWithResource: valid resource minting asset"
            pure $ pcon PTrue
        )
        (quantity #== 1)

    publisherIsSignatory <-
      pboolC
        (fail "parseOutputWithResource: publisher didn't sign the transaction")
        ( do
            ptraceC "parseOutputWithResource: publisher signed the transaction"
            pure $ pcon PTrue
        )
        (pelem # pdata publishedBy # sigs)

    sentToResourceValidator <-
      pboolC
        (fail "parseOutputWithResource: output not sent to resourceValidator")
        ( do
            ptraceC "parseOutputWithResource: output sent to resourceValidator"
            pure $ pcon PTrue
        )
        (outAddr #== pfield @"rmp'resourceValidatorAddress" # params)

    pboolC
      (fail "parseOutputWithResource: failed")
      (pure $ pcon PTrue)
      (hasSinglePublisherToken #&& publisherIsSignatory #&& sentToResourceValidator)
