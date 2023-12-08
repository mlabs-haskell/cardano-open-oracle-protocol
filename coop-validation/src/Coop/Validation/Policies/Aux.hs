module Coop.Validation.Policies.Aux (pmustSpendAtLeastAa, phashInput) where

import Coop.Validation.Aux (pfoldTxInputs, phasCurrency)
import Plutarch (pmatch, unTermCont)
import Plutarch.Api.V1.Value (PTokenName, pvalueOf)
import Plutarch.Api.V2 (PCurrencySymbol, PTuple, PTxInInfo)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Bool (pif)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.Extra.TermCont (pletC)
import Plutarch.Monadic qualified as P
import Plutarch.Num (PNum ((#+)))
import Plutarch.Prelude (ClosedTerm, PByteString, PInteger, PPair (PPair), PPartialOrd ((#<), (#<=)), pcon, pconsBS, pfield, phoistAcyclic, plam, plet, pletFields, ptrace, ptraceError, (#), (#$), type (:-->))
import Prelude (Applicative (pure), Monoid (mempty), Semigroup ((<>)), ($))

{- | Checks for total spent $AA tokens and create a unique bytestring from them

 - accumulate all spent $AA tokens and check if totals are at least as specified
 - create unique bytestring from $AA inputs by hashing the concatenation of (idx,id) pairs
-}
pmustSpendAtLeastAa :: ClosedTerm (PScriptContext :--> PTuple PCurrencySymbol PTokenName :--> PInteger :--> PByteString)
pmustSpendAtLeastAa = phoistAcyclic $
  plam $ \ctx aaAc atLeastAaQ -> ptrace "pmustSpendAtLeastAa" P.do
    aaCs <- plet $ pfield @"_0" # aaAc
    aaTn <- plet $ pfield @"_1" # aaAc

    let foldFn acc txIn = P.do
          -- check if $AA input
          txIn' <- pletFields @'["resolved", "outRef"] txIn
          txInVal <- plet $ pfield @"value" # txIn'.resolved
          pif
            (phasCurrency # aaCs # txInVal)
            ( ptrace "pmustSpendAtLeastAa: Found an $AA input" P.do
                PPair aaVal tnBytes <- pmatch acc
                -- accumulate token name bytes
                txId <- plet $ pfield @"_0" #$ pfield @"id" # txIn'.outRef
                txIdx <- plet $ pfield @"idx" # txIn'.outRef
                tnBytes' <- plet $ tnBytes <> pconsBS # txIdx # txId
                -- accumulate token quantities
                aaVal' <- plet $ aaVal #+ (pvalueOf # txInVal # aaCs # aaTn)

                pcon $ PPair aaVal' tnBytes'
            )
            (ptrace "pmustSpendAtLeastAa: Skipping non $AA input" acc)

    PPair aaTokensSpent tnBytes <- pmatch $ pfoldTxInputs # ctx # plam foldFn # pcon (PPair 0 mempty)

    pif
      (atLeastAaQ #<= aaTokensSpent)
      (ptrace "pmustSpendAtLeastAa: Spent at least the specified amount of AA tokens" $ pblake2b_256 # tnBytes)
      (ptraceError "pmustSpendAtLeastAa: Must spend at least the specified amount of AA tokens")

{- | `phashInput txInInfo` creates a unique bytestring from the given transaction input `txInInfo`

 Does blake2b_256 (ix:txId)
-}
phashInput :: ClosedTerm (PTxInInfo :--> PByteString)
phashInput = phoistAcyclic $
  plam $ \inInfo -> unTermCont $ do
    txId <- pletC $ pfield @"_0" # (pfield @"id" # (pfield @"outRef" # inInfo))
    txIdx <- pletC $ pfield @"idx" # (pfield @"outRef" # inInfo)
    pure $
      pif
        (txIdx #< 256)
        (pblake2b_256 # (pconsBS # txIdx # txId))
        (ptraceError "phashInput: Transaction output index must fit in an octet")
