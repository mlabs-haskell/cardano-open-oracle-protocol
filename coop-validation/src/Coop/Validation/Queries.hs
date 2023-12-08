module Coop.Validation.Queries (certificateIsDeleted, certificateIsCreated, certificateIsValid, getCertificate, getMain, getAuthenticationWithValidCert, authenticationIsDeleted) where

import Coop.Validation.Aux (punit)
import Coop.Validation.Aux qualified as Aux
import LambdaBuffers.Coop.Validation.Plutus.Plutarch qualified as P
import LambdaBuffers.Runtime.Plutarch (PAssetClass)
import Plutarch (POpaque, popaque, unTermCont, (#$))
import Plutarch.Api.V1.AssocMap (pfromAscList)
import Plutarch.Api.V1.Value (passertPositive, pvalueOf)
import Plutarch.Api.V1.Value qualified as PValue
import Plutarch.Api.V2 (PCurrencySymbol, PTxInInfo)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC, ptraceC)
import Plutarch.Num (PNum (pnegate))
import Plutarch.Prelude (ClosedTerm, PBuiltinList (PCons, PNil), PEq ((#==)), PList, PPair (PPair), PUnit, pcon, pconstant, pelemAt, pfield, pfoldr, pfromData, pfstBuiltin, phoistAcyclic, plam, pshow, (#), type (:-->))
import Prelude (Applicative (pure), Integer, ($), (.))

certificateIsCreated :: ClosedTerm (PCurrencySymbol :--> PScriptContext :--> P.CertificateId :--> POpaque)
certificateIsCreated = phoistAcyclic $ plam \certificatesSym ctx certId -> unTermCont $ do
  txMints <- pletC $ pfield @"mint" #$ pfield @"txInfo" # ctx

  P.CertificateId certIdTokenName <- pmatchC certId

  gotCertQuantity <- pletC $ pvalueOf # txMints # certificatesSym # pfromData certIdTokenName
  pguardC (Aux.punwords ["Expected 1 x $CERT[", pshow certId, "] but got", pshow gotCertQuantity]) $ gotCertQuantity #== 1

  pure $ popaque punit

certificateIsDeleted :: ClosedTerm (PCurrencySymbol :--> PScriptContext :--> P.CertificateId :--> POpaque)
certificateIsDeleted = phoistAcyclic $ plam \certificatesSym ctx certId -> unTermCont $ do
  txMints <- pletC $ pfield @"mint" #$ pfield @"txInfo" # ctx

  P.CertificateId certIdTokenName <- pmatchC certId

  gotCertQuantity <- pletC $ pvalueOf # txMints # certificatesSym # pfromData certIdTokenName
  pguardC (Aux.punwords ["Expected -1 x $CERT[", pshow certId, "] but got", pshow gotCertQuantity]) $ gotCertQuantity #== pnegate # 1

  pure $ popaque punit

getCertificate ::
  ClosedTerm
    ( PScriptContext
        :--> P.MainDatum
        :--> P.ReferenceLocation
        :--> P.CertificateId
        :--> P.CertificateDatum
    )
getCertificate = plam $ \ctx main certificateLocation certificateId -> unTermCont $ do
  references <- pletC $ pfromData $ pfield @"referenceInputs" # (pfield @"txInfo" # ctx)
  P.ReferenceLocation referenceIndex <- pmatchC certificateLocation
  certTxInInfo <- pletC $ pelemAt # pfromData referenceIndex # references
  txOut <- pletC . pfromData $ pfield @"resolved" # certTxInInfo

  P.MainDatum protocol _ _ <- pmatchC main
  P.Protocol _ _ _ certificatesSymbol _ <- pmatchC $ pfromData protocol

  value <- pletC $ pfield @"value" # txOut
  value' <- pletC $ Aux.pvalueWithoutAda # value
  P.CertificateId certIdTokenName <- pmatchC certificateId
  wantedValue <- pletC $ passertPositive #$ PValue.psingleton # pfromData certificatesSymbol # pfromData certIdTokenName # pconstant (1 :: Integer)
  pguardC (Aux.punwords ["Expected 1 x $CERT[", pshow certificateId, "] but got", pshow value']) $
    value' #== wantedValue

  ptraceC "Datum is CertificateDatum?"
  pure $ Aux.pdatumFromTxOut @P.CertificateDatum # ctx # txOut

certificateIsValid :: ClosedTerm (PScriptContext :--> P.CertificateDatum :--> PUnit)
certificateIsValid = phoistAcyclic $ plam \ctx certificate -> unTermCont $ do
  P.CertificateDatum validity _redeemerAsset <- pmatchC certificate

  certValidUntil <- pletC $ pfield @"_0" #$ pfield @"to" # validity
  ptraceC (Aux.punwords ["Transactions validates after", pshow certValidUntil, "?"])
  pure $ Aux.pmustValidateAfter # ctx # certValidUntil

{- | `authenticationIsDeleted # ctx # main # authToDelete` checks that -1 x $AUTH[certId] assets computed from `authToDelete` is in transaction mint.
 NOTE(bladyjoker): `authToDelete` must be sorted!
-}
authenticationIsDeleted :: ClosedTerm (PScriptContext :--> P.MainDatum :--> PList P.CertificateId :--> PUnit)
authenticationIsDeleted = phoistAcyclic $ plam \ctx main authToDelete -> unTermCont $ do
  ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
  mints <- pletC $ pfield @"mint" # ctx'.txInfo

  P.MainDatum protocol _ _ <- pmatchC main
  P.Protocol _ _ _ _ authenticationSymbol <- pmatchC $ pfromData protocol

  qtokenNames <- pletC $ Aux.plookupSymbol # mints # pfromData authenticationSymbol

  qtokenNamesList <-
    pletC $
      pfoldr
        # plam
          ( \certId qtokenNames' -> unTermCont $ do
              P.CertificateId certIdTokenName <- pmatchC certId
              pure $
                pcon $
                  PCons
                    (Aux.pqtokenName # pfromData certIdTokenName # (pnegate # pconstant (1 :: Integer)))
                    qtokenNames'
          )
        # pcon PNil
        # authToDelete

  qtokenNames' <- pletC $ pfromAscList # qtokenNamesList

  pguardC (Aux.punwords ["Expected $AUTH tokens in transaction mint", pshow qtokenNames, "to match the provided", pshow qtokenNames']) $
    qtokenNames #== qtokenNames

  pure punit

getAuthenticationWithValidCert :: ClosedTerm (PScriptContext :--> P.MainDatum :--> P.InputLocation :--> P.ReferenceLocation :--> PPair PTxInInfo P.CertificateId)
getAuthenticationWithValidCert = phoistAcyclic $ plam \ctx main authLocation certLocation -> unTermCont $ do
  ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
  inputs <- pletC $ pfield @"inputs" # ctx'.txInfo

  P.InputLocation inputIndex <- pmatchC authLocation
  ptraceC $ Aux.punwords ["Authentication is in transaction inputs at", pshow inputIndex, "?"]
  authTxInInfo <- pletC $ pelemAt # pfromData inputIndex # pfromData inputs
  authTxOut <- pletC $ pfromData $ pfield @"resolved" # authTxInInfo
  authValue <- pletC $ pfield @"value" # authTxOut

  P.MainDatum protocol _ _ <- pmatchC main
  P.Protocol _ _ _ _ authenticationSymbol <- pmatchC $ pfromData protocol

  -- NOTE(bladyjoker): We only take the first $AUTH[certificateId] from Authentication input.
  ptraceC $ Aux.punwords ["Getting the first 1 x $AUTH from the spent Authentication input"]
  PCons qtokenName _rest <- pmatchC $ Aux.plookupSymbol # authValue # pfromData authenticationSymbol
  certificateId <- pletC $ pcon $ P.CertificateId $ pfstBuiltin # qtokenName

  ptraceC $ Aux.punwords ["Certificate with id", pshow certificateId, "is available?"]
  certificate <- pletC $ getCertificate # ctx # main # certLocation # certificateId

  ptraceC "Certificate is valid?"
  _ <- pletC $ certificateIsValid # ctx # certificate

  pure $ pcon $ PPair authTxInInfo certificateId

-- | TODO(bladyjoker): Optimize with ReferenceLocation
getMain :: ClosedTerm (PScriptContext :--> P.ReferenceLocation :--> PAssetClass :--> P.MainDatum)
getMain = phoistAcyclic $ plam \ctx refLoc mainAsset -> unTermCont $ do
  references <- pletC $ pfromData $ pfield @"referenceInputs" # (pfield @"txInfo" # ctx)
  P.ReferenceLocation referenceIndex <- pmatchC refLoc
  mainTxInInfo <- pletC $ pelemAt # pfromData referenceIndex # references
  txOut <- pletC . pfromData $ pfield @"resolved" # mainTxInInfo

  value <- pletC $ pfield @"value" # txOut
  value' <- pletC $ Aux.pvalueWithoutAda # value
  wantedValue <- pletC $ passertPositive #$ Aux.passetClassValue # mainAsset # pconstant (1 :: Integer)
  pguardC (Aux.punwords ["Expected 1 x $MAIN", pshow wantedValue, "but got", pshow value']) $
    value' #== wantedValue

  ptraceC "Datum is MainDatum?"
  pure $ Aux.pdatumFromTxOut @P.MainDatum # ctx # txOut
