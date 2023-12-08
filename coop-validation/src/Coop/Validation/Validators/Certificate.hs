module Coop.Validation.Validators.Certificate (mkValidator) where

import Coop.Validation.Aux qualified as Aux
import Coop.Validation.Queries qualified as Queries
import LambdaBuffers.Coop.Validation.Plutus.Plutarch qualified as P
import LambdaBuffers.Runtime.Plutarch (PAssetClass)
import Plutarch (ClosedTerm, pcon, perror, phoistAcyclic, plam, unTermCont, (#), (:-->))
import Plutarch.Api.V2 (PValidator)
import Plutarch.Extra.TermCont (pletC, pmatchC, ptraceC)
import Plutarch.Prelude (PAsData, pdata, pfield, pfromData, pshow, ptrace)

mkValidator :: ClosedTerm (PAsData PAssetClass :--> PValidator)
mkValidator = phoistAcyclic $
  plam $ \mainAsset _dat rdmr ctx -> ptrace "[@Certs certificate validator]" $ unTermCont $ do
    P.CertificateValidatorRedeemer mainLocation action <- pmatchC $ pfromData (Aux.ptryFromData @P.CertificateValidatorRedeemer rdmr)

    ptraceC $ Aux.punwords ["Main is in transaction references at", pshow mainLocation, "?"]
    P.MainDatum protocol _ _ <- pmatchC $ Queries.getMain # ctx # pfromData mainLocation # pfromData mainAsset
    P.Protocol _ _ _ certificatesSymbol _ <- pmatchC $ pfromData protocol

    P.CertificateValidatorAction'Delete <- pmatchC $ pfromData action
    ptraceC $ Aux.punwords ["Deleting Certificate"]

    ptraceC $ Aux.punwords ["Finding Certificate identifier in the spent input"]
    txInInfo <- pletC $ Aux.pfindOwnInput # ctx
    txOut <- pletC . pfromData $ pfield @"resolved" # txInInfo
    value <- pletC $ pfield @"value" # txOut
    certTokenName <- pletC $ Aux.pgetTokenNameIfSingle # value # pfromData certificatesSymbol

    certificateId <- pletC $ pcon $ P.CertificateId (pdata certTokenName)
    ptraceC $ Aux.punwords ["Certificate deletion is validated by $CERT policy?"]
    _ <- pletC $ Queries.certificateIsDeleted # pfromData certificatesSymbol # ctx # certificateId
    pure perror
