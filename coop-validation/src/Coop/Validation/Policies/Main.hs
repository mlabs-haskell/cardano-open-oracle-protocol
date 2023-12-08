module Coop.Validation.Policies.Main (mkPolicy) where

import Coop.Validation.Aux qualified as Aux
import Coop.Validation.Policies.Aux qualified as Aux
import LambdaBuffers.Coop.Validation.Plutus.Plutarch qualified as P
import Plutarch (ClosedTerm, pcon, phoistAcyclic, plam, pmatch, popaque, unTermCont, (#), (:-->))
import Plutarch.Api.V2 (PMintingPolicy, PTokenName (PTokenName), PTxOutRef)
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC, ptraceC)
import Plutarch.Num (PNum (pnegate))
import Plutarch.Prelude (PBuiltinList (PCons, PNil), PEq ((#==)), pconstant, pelemAt, pfield, pfromData, pshow, psndBuiltin, ptrace)

mkPolicy ::
  ClosedTerm
    ( PTxOutRef :--> PMintingPolicy
    )
mkPolicy = phoistAcyclic $
  plam $ \txOutRef rdmr ctx ->
    ptrace "[$MAIN policy]" $ unTermCont $ do
      ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
      txInfo <- pletFieldsC @'["inputs", "mint"] ctx'.txInfo
      inputs <- pletC $ pfromData txInfo.inputs
      ownSym <- pletC $ Aux.pownCurrencySymbol # ctx'.purpose

      pure $ pmatch
        (pfromData (Aux.ptryFromData @P.MainPolicyRedeemer rdmr))
        \case
          P.MainPolicyRedeemer'Create inputLocation -> unTermCont $ do
            ptraceC $ Aux.punwords ["Validating creation of $MAIN with TxOutRef configuration", pshow txOutRef]

            P.InputLocation inputIndex <- pmatchC $ pfromData inputLocation
            ptraceC $ Aux.punwords ["One shot input is in transaction inputs at", pshow inputIndex, "?"]
            txInInfo <- pletC $ pelemAt # pfromData inputIndex # inputs

            txOutRef' <- pletC $ pfromData $ pfield @"outRef" # txInInfo
            pguardC (Aux.punwords ["Expected TxOutRef", pshow txOutRef, "but got", pshow txOutRef']) $ txOutRef #== txOutRef'

            ownQTokenNamesMinted <- pletC $ Aux.plookupSymbol # txInfo.mint # ownSym
            computedTokenName <- pletC $ pcon $ PTokenName $ Aux.phashInput # txInInfo
            pguardC (Aux.punwords ["Expected transaction to mint 1 x $MAIN as was computed", pshow computedTokenName, "vs minted", pshow ownQTokenNamesMinted]) $
              ownQTokenNamesMinted #== Aux.plistSingleton # (Aux.pqtokenName # computedTokenName # 1)

            pure $ popaque Aux.punit
          P.MainPolicyRedeemer'Delete -> unTermCont $ do
            ptraceC "Validating deletion of $MAIN"

            PCons ownQTokenNamesMinted rest <- pmatchC $ Aux.plookupSymbol # txInfo.mint # ownSym

            gotQuantity <- pletC . pfromData $ psndBuiltin # ownQTokenNamesMinted
            pguardC (Aux.punwords ["Wanted -1 x $MAIN but got", pshow gotQuantity]) $
              gotQuantity #== (pnegate # pconstant (1 :: Integer))

            pguardC (Aux.punwords ["Expected only a single quantified $MAIN but got", pshow rest]) $
              rest #== pcon PNil

            pure $ popaque Aux.punit
