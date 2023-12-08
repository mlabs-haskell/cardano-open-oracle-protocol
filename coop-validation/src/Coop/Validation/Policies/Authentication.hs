module Coop.Validation.Policies.Authentication (
  mkPolicy,
)
where

import Coop.Validation.Aux (ptryFromData, punit)
import Coop.Validation.Aux qualified as Aux
import Coop.Validation.Queries qualified as Queries
import LambdaBuffers.Coop.Validation.Plutus.Plutarch qualified as P
import LambdaBuffers.Runtime.Plutarch (PAssetClass, PList)
import Plutarch (POpaque, pmatch, popaque, unTermCont, (#$))
import Plutarch.Api.V1.Value (PTokenName, passertPositive, psingleton)
import Plutarch.Api.V2 (PCurrencySymbol, PMintingPolicy, PTxInInfo)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Bool (PEq ((#==)))
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC, ptraceC)
import Plutarch.Num (PNum (pnegate))
import Plutarch.Prelude (ClosedTerm, PAsData, PBuiltinList (PCons, PNil), PBuiltinPair, PInteger, pcon, pelemAt, pfield, pfoldr, pfromData, phoistAcyclic, plam, pshow, ptrace, (#), type (:-->))
import Prelude (Applicative (pure), ($))

-- | \$AUTH policy
mkPolicy :: ClosedTerm (PAsData PAssetClass :--> PMintingPolicy)
mkPolicy = phoistAcyclic $
  plam $ \mainAsset red ctx -> ptrace "[$AUTH policy]" $ unTermCont $ do
    P.AuthenticationPolicyRedeemer mainLocation actions <- pmatchC $ pfromData (ptryFromData @P.AuthenticationPolicyRedeemer red)

    ptraceC $ Aux.punwords ["Main is in transaction references at", pshow mainLocation, "?"]
    main <- pletC $ Queries.getMain # ctx # pfromData mainLocation # pfromData mainAsset

    pure $ validateActions # main # ctx # pfromData actions

validateActions :: ClosedTerm (P.MainDatum :--> PScriptContext :--> PList P.AuthenticationPolicyAction :--> POpaque)
validateActions = phoistAcyclic $
  ptrace "[actions validation]" $
    plam $ \main ctx actions -> unTermCont $ do
      ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
      inputs <- pletC $ pfield @"inputs" # ctx'.txInfo
      mints <- pletC $ pfield @"mint" # ctx'.txInfo
      ownSym <- pletC $ Aux.pownCurrencySymbol # ctx'.purpose

      certIdTokenNamesToMintOrBurn' <-
        pletC $
          pfoldr
            # plam
              ( \act certIdTokenNamesToMintOrBurn -> unTermCont $ do
                  pure $ pmatch act $ \case
                    P.AuthenticationPolicyAction'Delete authDelete ->
                      pcon $
                        PCons
                          (delete # ctx # ownSym # pfromData inputs # pfromData authDelete)
                          certIdTokenNamesToMintOrBurn
                    P.AuthenticationPolicyAction'Create authCreate ->
                      pcon $
                        PCons
                          (create # main # ctx # pfromData authCreate)
                          certIdTokenNamesToMintOrBurn
              )
            # pcon PNil
            # actions

      certIdTokenNamesMinted <- pletC $ Aux.plookupSymbol # pfromData mints # ownSym

      pguardC
        ( Aux.punwords
            [ "Expected transaction to mint $AUTH assets"
            , pshow certIdTokenNamesToMintOrBurn'
            , "but instead transaction minted"
            , pshow certIdTokenNamesMinted
            , "Did you forget to align the order of 'actions' in AuthenticationPolicyRedeemer with the transaction mint?"
            ]
        )
        $ certIdTokenNamesMinted #== certIdTokenNamesToMintOrBurn'

      pure $ popaque punit

{- | `delete ctx ownSym inputs deleteAuth` validates deleting of an Authentication (found at `inputLocation` @ `deleteAuth.authenticationLocation`) and burning of `deleteAuth.quantity` x $AUTH[`deleteAuth.certificateId`] assets ($AUTH symbol being provided by `ownSym`).

 1. Let `authInput` be the input at `inputLocation` in `inputs`,
 2. `authInput` non-$ADA value must be `deleteAuth.quantity` x $AUTH[`deleteAuth.certificateId`] where `symbolOf $AUTH` is provided by `ownSym`,
 3. Returns the -1 x `deleteAuth.quantity` x $AUTH[`deleteAuth.certificateId`] to burn.
-}
delete :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PBuiltinList PTxInInfo :--> P.DeleteAuthentication :--> PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
delete = phoistAcyclic $
  plam $ \_ctx ownSym inputs deleteAuthentication ->
    unTermCont $
      do
        P.DeleteAuthentication inputLocation certId quantity <- pmatchC deleteAuthentication
        ptraceC $ Aux.punwords ["Validating deletion of an Authentication with id", pshow certId]

        P.InputLocation inputIndex <- pmatchC $ pfromData inputLocation
        ptraceC $ Aux.punwords ["Authentication is in transaction inputs at", pshow inputIndex, "?"]
        txInInfo <- pletC $ pelemAt # pfromData inputIndex # inputs
        txOut <- pletC $ pfromData $ pfield @"resolved" # txInInfo
        txOut'value <- pletC $ pfield @"value" # txOut

        valueWithoutAda <- pletC $ Aux.pvalueWithoutAda # txOut'value
        P.CertificateId certIdTokenName <- pmatchC $ pfromData certId
        wantValue <- pletC $ passertPositive #$ psingleton # ownSym # pfromData certIdTokenName # pfromData quantity
        pguardC
          (Aux.punwords ["Expected to find", pshow quantity, "x $AUTH[", pshow certId, "], but got", pshow valueWithoutAda])
          $ valueWithoutAda #== wantValue

        pure $ Aux.pdpair (pfromData certIdTokenName) (pnegate # pfromData quantity)

{- | `create main ctx createAuth` validates creation of an Authentication and minting of `createAuth.quantity` x $AUTH[`createAuth.certificateId`] assets ($AUTH symbol being provided by `ownSym`).

 1. Checks that a Certificate with `createAuth.certificateId` is created in the transaction,
 2. Returns the `ceateAuth.quantity` x $AUTH[`createAuth.certificateId`] to mint.
-}
create :: ClosedTerm (P.MainDatum :--> PScriptContext :--> P.CreateAuthentication :--> PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
create = phoistAcyclic $
  plam $ \main ctx createAuth ->
    unTermCont $
      do
        P.CreateAuthentication certId quantity <- pmatchC createAuth
        ptraceC $ Aux.punwords ["Validating creation of an Authentication with id", pshow certId]

        P.MainDatum protocol _ _ <- pmatchC main
        P.Protocol _ _ _ certificatesSymbol _ <- pmatchC $ pfromData protocol

        ptraceC $ Aux.punwords ["Certificate with id", pshow certId, "is created?"]
        P.CertificateId certIdTokenName <- pmatchC $ pfromData certId
        _ <- pletC $ Queries.certificateIsCreated # pfromData certificatesSymbol # ctx # pfromData certId

        pure $ Aux.pdpair (pfromData certIdTokenName) (pnegate # pfromData quantity)
