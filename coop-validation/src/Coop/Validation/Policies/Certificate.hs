module Coop.Validation.Policies.Certificate (mkPolicy)
where

import Coop.Validation.Aux (ptryFromData, punit)
import Coop.Validation.Aux qualified as Aux
import Coop.Validation.Policies.Aux qualified as Aux
import Coop.Validation.Queries qualified as Queries
import LambdaBuffers.Coop.Validation.Plutus.Plutarch qualified as P
import LambdaBuffers.Runtime.Plutarch (PAssetClass, PList)
import Plutarch (POpaque, pmatch, popaque, unTermCont, (#$))
import Plutarch.Api.V1.Value (PTokenName (PTokenName), passertPositive, psingleton)
import Plutarch.Api.V2 (PCurrencySymbol, PMintingPolicy, PTxInInfo, PTxOut)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC, ptraceC)
import Plutarch.Num (PNum (pnegate))
import Plutarch.Prelude (ClosedTerm, PAsData, PBuiltinList (PCons, PNil), PEq ((#==)), pcon, pconstant, pelemAt, pfield, pfoldr, pfromData, phoistAcyclic, plam, pshow, ptrace, (#), type (:-->))
import Prelude (Applicative (pure), Integer, ($))

-- | \$CERT minting policy that validates creation and deletion of Certificates
mkPolicy :: ClosedTerm (PAsData PAssetClass :--> PMintingPolicy)
mkPolicy = phoistAcyclic $
  plam $ \mainAsset red ctx -> ptrace "[$CERT policy]" $ unTermCont $ do
    P.CertificatePolicyRedeemer mainLocation actions <- pmatchC $ pfromData (ptryFromData @P.CertificatePolicyRedeemer red)

    ptraceC $ Aux.punwords ["Main is in transaction references at", pshow mainLocation, "?"]
    main <- pletC $ Queries.getMain # ctx # pfromData mainLocation # pfromData mainAsset

    pure $ validateActions # main # ctx # pfromData actions

validateActions :: ClosedTerm (P.MainDatum :--> PScriptContext :--> PList P.CertificatePolicyAction :--> POpaque)
validateActions = phoistAcyclic $
  ptrace "[actions validation]" $
    plam $ \main ctx actions -> unTermCont $ do
      ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
      inputs <- pletC $ pfield @"inputs" # ctx'.txInfo
      outputs <- pletC $ pfield @"outputs" # ctx'.txInfo
      mints <- pletC $ pfield @"mint" # ctx'.txInfo
      ownSym <- pletC $ Aux.pownCurrencySymbol # ctx'.purpose

      certIdTokenNamesToMintOrBurn' <-
        pletC $
          pfoldr
            # plam
              ( \act certIdTokenNamesToMintOrBurn -> unTermCont $ do
                  pure $ pmatch act $ \case
                    P.CertificatePolicyAction'Delete inputLocation certId ->
                      pcon $
                        PCons
                          ( Aux.pdpair
                              (delete # ctx # ownSym # pfromData inputs # pfromData certId # pfromData inputLocation)
                              (pnegate # pconstant (1 :: Integer))
                          )
                          certIdTokenNamesToMintOrBurn
                    P.CertificatePolicyAction'Create outputLocation certId ->
                      pcon $
                        PCons
                          ( Aux.pdpair
                              (create # main # ctx # ownSym # pfromData outputs # pfromData certId # pfromData outputLocation)
                              (pconstant (1 :: Integer))
                          )
                          certIdTokenNamesToMintOrBurn
              )
            # pcon PNil
            # actions

      certIdTokenNamesMinted <- pletC $ Aux.plookupSymbol # pfromData mints # ownSym

      pguardC
        ( Aux.punwords
            [ "Expected transaction to mint $CERT assets"
            , pshow certIdTokenNamesToMintOrBurn'
            , "but instead transaction minted"
            , pshow certIdTokenNamesMinted
            , "Did you forget to align the order of 'actions' in CertificatePolicyRedeemer with the transaction mint?"
            ]
        )
        $ certIdTokenNamesMinted #== certIdTokenNamesToMintOrBurn'

      pure $ popaque punit

{- | `delete ctx ownSym inputs certId inputLocation` validates deleting of a Certificate (found at `inputLocation` @ `inputs`) and burning of $CERT[`certId`] asset ($CERT symbol being provided by `ownSym`).

 1. Let `certInput` be the input at `inputLocation` in `inputs`,
 2. `certInput` non-$ADA value must be 1 x $CERT[`certId`] where `symbolOf $CERT` is provided by `ownSym`,
 3. `certInput` datum must be `CertificateDatum` and let it be `certDatum`,
 4. Transaction must validate after `certDatum.validity`,
 5. Tranasction must spend at least 1 x $CERT_RDMR asset as specified in `certDatum.redeemerAsset`,
 6. Returns the $CERT token name to burn.
-}
delete :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PBuiltinList PTxInInfo :--> P.CertificateId :--> P.InputLocation :--> PTokenName)
delete = phoistAcyclic $
  plam $ \ctx ownSym inputs certId inputLocation ->
    unTermCont $
      do
        ptraceC $ Aux.punwords ["Validating deletion of a Certificate with id", pshow certId]

        P.InputLocation inputIndex <- pmatchC inputLocation
        ptraceC $ Aux.punwords ["Certificate is in transaction inputs at", pshow inputIndex, "?"]
        txInInfo <- pletC $ pelemAt # pfromData inputIndex # inputs
        txOut <- pletC $ pfromData $ pfield @"resolved" # txInInfo
        txOut'value <- pletC $ pfield @"value" # txOut

        valueWithoutAda <- pletC $ Aux.pvalueWithoutAda # txOut'value
        P.CertificateId certIdTokenName <- pmatchC certId
        wantValue <- pletC $ passertPositive #$ psingleton # ownSym # pfromData certIdTokenName # pconstant 1
        pguardC
          (Aux.punwords ["Expected to find 1 x $CERT[", pshow certId, "], but got", pshow valueWithoutAda])
          $ valueWithoutAda #== wantValue

        ptraceC "Datum is CertificateDatum?"
        P.CertificateDatum validity redeemerAsset <- pmatchC $ Aux.pdatumFromTxOut @P.CertificateDatum # ctx # txOut

        certValidUntil <- pletC $ pfield @"_0" #$ pfield @"to" # validity
        ptraceC (Aux.punwords ["Transactions validates after", pshow certValidUntil, "?"])
        _ <- pletC $ Aux.pmustValidateAfter # ctx # certValidUntil

        -- TODO(perf): Consider adding a TxOutRef to the redeemer as it would make it more efficient to find the $CERT-RDMR input
        _ <- pletC $ Aux.pmustSpendAtLeast # ctx # (pfield @"_0" # pfromData redeemerAsset) # (pfield @"_1" # pfromData redeemerAsset)

        pure $ pfromData certIdTokenName

{- | `create main ctx ownSym outputs certId outputLocation` validates creation of a Certificate (found at `outputLocation` @ `outputs`) and minting of $CERT[`certId`] asset ($CERT symbol being provided by `ownSym`).

 1. Let `certOutput` be the output at `outputLocation` in `outputs`,
 2. `certOutput` non-$ADA value must be 1 x $CERT[`certId`] where `symbolOf $CERT` is provided by `ownSym`,
 3. `certOutput` datum must be `CertificateDatum` and let it be `certDatum`,
 4. `certOutput` address must be equal to `main.protocol.certificatesAddress`,
 5. `main.requiredAtLeast` x $AA (`main.authenticationAuthorityAsset`) assets must be spent,
 6. Let `uniqueBytesFromAaIns` be formed by traversing $AA inputs, accumulating (ix, txId) pairs and applying `blake2b_256` hash on the result,
 7. `uniqueBytesFromAaIns` must match the `certId`.
 8. Returns the $CERT token name to mint.
-}
create :: ClosedTerm (P.MainDatum :--> PScriptContext :--> PCurrencySymbol :--> PBuiltinList PTxOut :--> P.CertificateId :--> P.OutputLocation :--> PTokenName)
create = phoistAcyclic $
  plam $ \main ctx ownSym outputs certId outputsLocation ->
    unTermCont $
      do
        ptraceC $
          Aux.punwords
            [ "Validating creation of a Certificate with id"
            , pshow certId
            ]

        P.MainDatum protocol authenticationAuthorityAsset requiredAtLeast <- pmatchC main
        P.Protocol _ certificatesAddress _ _ _ <- pmatchC $ pfromData protocol

        ptraceC $ Aux.punwords ["Spends at least", pshow requiredAtLeast, "of of $AA tokens", pshow authenticationAuthorityAsset, "?"]
        uniqueBytesFromAaIns <- pletC $ Aux.pmustSpendAtLeastAa # ctx # pfromData authenticationAuthorityAsset # pfromData requiredAtLeast

        P.OutputLocation outputIndex <- pmatchC outputsLocation
        ptraceC $
          Aux.punwords
            [ "Looking up the Certificate in transaction outputs at"
            , pshow outputIndex
            ]
        txOut <- pletC $ pelemAt # pfromData outputIndex # outputs

        certsAddressGot <- pletC $ pfield @"address" # txOut
        pguardC (Aux.punwords ["Expected the @Certs address to be", pshow certificatesAddress, "but got", pshow certsAddressGot]) $ certsAddressGot #== pfromData certificatesAddress

        valueWithoutAda <- pletC $ Aux.pvalueWithoutAda # (pfield @"value" # txOut)
        P.CertificateId certIdTokenName <- pmatchC certId
        wantValue <- pletC $ passertPositive #$ psingleton # ownSym # pfromData certIdTokenName # pconstant 1
        pguardC
          (Aux.punwords ["Expected to find 1 x $CERT[", pshow certId, "], but got", pshow valueWithoutAda])
          $ valueWithoutAda #== wantValue

        computedTokenName <- pletC $ pcon (PTokenName uniqueBytesFromAaIns)
        pguardC (Aux.punwords ["Expected the computed Certificate identifiers", pshow computedTokenName, "to match the one in the Certificate TxOut", pshow certIdTokenName]) $ pfromData certIdTokenName #== pcon (PTokenName uniqueBytesFromAaIns)

        ptraceC "Datum is CertificateDatum?"
        P.CertificateDatum _validity _redeemerAsset <- pmatchC $ Aux.pdatumFromTxOut @P.CertificateDatum # ctx # txOut

        pure computedTokenName
