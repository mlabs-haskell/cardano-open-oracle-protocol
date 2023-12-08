module Coop.Validation.Policies.FactStatement (
  mkPolicy,
)
where

import Coop.Validation.Aux qualified as Aux
import Coop.Validation.Policies.Aux qualified as Aux
import Coop.Validation.Queries qualified as Queries
import LambdaBuffers.Coop.Validation.Plutus.Plutarch qualified as P
import LambdaBuffers.Runtime.Plutarch (PAssetClass, PList)
import Plutarch (POpaque, pmatch, popaque, unTermCont, (#$))
import Plutarch.Api.V1.Value (PTokenName (PTokenName), passertPositive)
import Plutarch.Api.V1.Value qualified as PValue
import Plutarch.Api.V2 (PCurrencySymbol, PMintingPolicy, PTxInInfo, PTxOut)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC, ptraceC)
import Plutarch.Num (PNum (pnegate))
import Plutarch.Prelude (ClosedTerm, PAsData, PBuiltinList (PCons, PNil), PEq ((#==)), PListLike (pcons, pnil), PMaybe (PNothing), PPair (PPair), pcon, pconstant, pelemAt, pfield, pfind, pfoldr, pfromData, phoistAcyclic, plam, pshow, ptrace, (#), type (:-->))
import Prelude (Applicative (pure), Integer, ($))

mkPolicy :: ClosedTerm (PAsData PAssetClass :--> PMintingPolicy)
mkPolicy = phoistAcyclic $
  plam $ \mainAsset red ctx -> ptrace "[$FS policy]" $ unTermCont $ do
    P.FactStatementPolicyRedeemer mainLocation actions <- pmatchC $ pfromData (Aux.ptryFromData @P.FactStatementPolicyRedeemer red)

    ptraceC $ Aux.punwords ["Main is in transaction references at", pshow mainLocation, "?"]
    main <- pletC $ Queries.getMain # ctx # pfromData mainLocation # pfromData mainAsset

    pure $ validateActions # main # ctx # pfromData actions

validateActions :: ClosedTerm (P.MainDatum :--> PScriptContext :--> PList P.FactStatementPolicyAction :--> POpaque)
validateActions = phoistAcyclic $
  ptrace "[$FS actions]" $
    plam $ \main ctx actions -> unTermCont $ do
      ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
      inputs <- pletC $ pfield @"inputs" # ctx'.txInfo
      outputs <- pletC $ pfield @"outputs" # ctx'.txInfo
      mints <- pletC $ pfield @"mint" # ctx'.txInfo
      ownSym <- pletC $ Aux.pownCurrencySymbol # ctx'.purpose

      PPair qfsIdTokenNamesToMint authToDeleteAndCount <-
        pmatchC $
          pfoldr
            # plam
              ( \act acc -> unTermCont $ do
                  PPair qfsIdTokenNamesToMint authToDeleteAndLocationsSeen <- pmatchC acc
                  PPair authToDelete authLocationsSeen <- pmatchC authToDeleteAndLocationsSeen
                  pure $ pmatch act $ \case
                    P.FactStatementPolicyAction'Delete deleteFactStatement -> unTermCont $ do
                      fsIdTokenName <- pletC $ delete # ctx # ownSym # pfromData inputs # pfromData deleteFactStatement
                      qfsIdTokenName <- pletC $ Aux.pqtokenName # fsIdTokenName # (pnegate # pconstant (1 :: Integer))
                      pure $
                        pcon $
                          PPair
                            (pcon $ PCons qfsIdTokenName qfsIdTokenNamesToMint)
                            authToDeleteAndLocationsSeen
                    P.FactStatementPolicyAction'Create createFactStatement -> unTermCont $ do
                      PPair fsIdTokenName certIdAndSeen <- pmatchC $ create # main # ctx # ownSym # pfromData outputs # authLocationsSeen # pfromData createFactStatement
                      PPair certId authLocationsSeen' <- pmatchC certIdAndSeen
                      qfsIdTokenName <- pletC $ Aux.pqtokenName # fsIdTokenName # pconstant (1 :: Integer)
                      pure $
                        pcon $
                          PPair
                            (pcon $ PCons qfsIdTokenName qfsIdTokenNamesToMint)
                            (pcon $ PPair (pcons # certId # authToDelete) authLocationsSeen')
              )
            # pcon (PPair (pcon PNil) (pcon $ PPair pnil pnil))
            # actions

      mintedFsIdTokenNames <- pletC $ Aux.plookupSymbol # pfromData mints # ownSym

      pguardC
        ( Aux.punwords
            [ "Expected transaction to mint $FS assets"
            , pshow qfsIdTokenNamesToMint
            , "but instead transaction minted"
            , pshow mintedFsIdTokenNames
            , "Did you forget to align the order of 'actions' in FactStatementPolicyRedeemer with the transaction mint?"
            ]
        )
        $ mintedFsIdTokenNames #== qfsIdTokenNamesToMint

      PPair authToDelete _ <- pmatchC authToDeleteAndCount

      ptraceC "Authentication is deleted?"
      _ <- pletC $ Queries.authenticationIsDeleted # ctx # main # authToDelete

      pure $ popaque Aux.punit

create :: ClosedTerm (P.MainDatum :--> PScriptContext :--> PCurrencySymbol :--> PBuiltinList PTxOut :--> PList P.InputLocation :--> P.CreateFactStatement :--> PPair PTokenName (PPair P.CertificateId (PList P.InputLocation)))
create = phoistAcyclic $
  plam $ \main ctx ownSym outputs authLocationsSeen createFactStatement ->
    unTermCont $
      do
        P.CreateFactStatement fsId factStatementLoc authLoc certLoc <- pmatchC createFactStatement
        ptraceC $ Aux.punwords ["Validating creation of a FactStatement with id", pshow fsId]

        P.OutputLocation outputsIndex <- pmatchC $ pfromData factStatementLoc
        ptraceC $ Aux.punwords ["FactStatement in transaction outputs at", pshow outputsIndex]
        txOut <- pletC $ pelemAt # pfromData outputsIndex # outputs

        P.MainDatum protocol _ _ <- pmatchC main
        P.Protocol factsAddr _ _ _ _ <- pmatchC $ pfromData protocol

        pguardC (Aux.punwords ["Expected transaction output to be at @Facts address", pshow factsAddr, "but got", pshow $ pfield @"address" # txOut]) $
          pfromData factsAddr #== pfield @"address" # txOut

        valueWithoutAda <- pletC $ Aux.pvalueWithoutAda # (pfield @"value" # txOut)
        P.FactStatementId fsIdTokenName <- pmatchC $ pfromData fsId
        wantedValue <- pletC $ passertPositive #$ PValue.psingleton # ownSym # pfromData fsIdTokenName # pconstant 1
        pguardC
          (Aux.punwords ["Expected to find 1 x $FS[", pshow fsId, "], but got", pshow valueWithoutAda])
          $ valueWithoutAda #== wantedValue

        ptraceC "Datum is FactStatementDatum?"
        P.FactStatementDatum _fact _externalId _gcAfter _submitter <- pmatchC $ Aux.pdatumFromTxOut @P.FactStatementDatum # ctx # txOut

        ptraceC $ Aux.punwords ["Getting Authentication in transaction inputs at", pshow authLoc, "that has valid Certificate in transaction references at", pshow certLoc]
        PPair authTxInInfo certId <- pmatchC $ Queries.getAuthenticationWithValidCert # ctx # main # pfromData authLoc # pfromData certLoc

        fsIdTokenName' <- pletC $ pcon $ PTokenName (Aux.phashInput # authTxInInfo)
        pguardC (Aux.punwords ["Expected the fact statement id provided in the redeemer", pshow fsIdTokenName, "to match the id in the transaction output", pshow fsIdTokenName']) $
          pfromData fsIdTokenName #== fsIdTokenName'

        ptraceC "Authentication input used more than once?"
        PNothing <- pmatchC $ pfind # plam (\authLocSeen -> authLocSeen #== pfromData authLoc) # authLocationsSeen

        pure $
          pcon $
            PPair
              (pfromData fsIdTokenName)
              (pcon $ PPair certId (pcons # pfromData authLoc # authLocationsSeen))

delete :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PBuiltinList PTxInInfo :--> P.DeleteFactStatement :--> PTokenName)
delete = phoistAcyclic $
  plam $ \ctx ownSym inputs deleteFactStatement ->
    unTermCont $
      do
        P.DeleteFactStatement fsId factStatementLocation <- pmatchC deleteFactStatement
        ptraceC $ Aux.punwords ["Validating deletion of a FactStatement with id", pshow fsId]

        P.InputLocation inputIndex <- pmatchC $ pfromData factStatementLocation
        ptraceC $ Aux.punwords ["FactStatement is in transaction inputs at", pshow inputIndex, "?"]
        txInInfo <- pletC $ pelemAt # pfromData inputIndex # inputs
        txOut <- pletC $ pfromData $ pfield @"resolved" # txInInfo

        ptraceC "1 x $FS asset in the input?"
        fsIdTokenName <- pletC $ Aux.pgetTokenNameIfSingle # (pfield @"value" # txOut) # ownSym

        P.FactStatementId fsIdTokenName' <- pmatchC $ pfromData fsId
        pguardC (Aux.punwords ["Expected the redeemer provided fact statement id", pshow fsId, "to match the input fact statement id", pshow fsIdTokenName]) $
          fsIdTokenName #== pfromData fsIdTokenName'

        ptraceC "Datum is FactStatementDatum?"
        P.FactStatementDatum _fact _externalId gcAfter submitter <- pmatchC $ Aux.pdatumFromTxOut @P.FactStatementDatum # ctx # txOut

        ptraceC (Aux.punwords ["Submitter", pshow $ pfromData submitter, "signed?"])
        _ <- pletC $ Aux.pmustBeSignedBy # ctx # pfromData submitter

        ptraceC (Aux.punwords ["Transaction validates after", pshow $ pfromData gcAfter, "?"])
        _ <- pletC $ Aux.pmustValidateAfter # ctx # pfromData gcAfter

        pure fsIdTokenName
