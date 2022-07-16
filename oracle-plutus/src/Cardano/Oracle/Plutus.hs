module Cardano.Oracle.Plutus (exampleFunction, exampleValidator, exampleMintingPolicy) where

import Plutarch (Config, popaque)
import Plutarch.Api.V1 (mkMintingPolicy, mkValidator)
import Plutarch.Prelude (
  PInteger,
  Term,
  pconstant,
  plam,
  plet,
  type (:-->),
 )
import Plutarch.TermCont (TermCont, tcont, unTermCont)
import Plutarch.Trace (ptrace)
import PlutusLedgerApi.V1.Scripts (MintingPolicy, Validator)

pletC :: Term s a -> TermCont s (Term s a)
pletC = tcont . plet

exampleFunction :: Term s (PInteger :--> PInteger)
exampleFunction = plam $ \x -> ptrace "exampleFunctions" $
  unTermCont $ do
    x' <- pletC $ x + 1 + 2 + 3
    return $ x' + x'

exampleValidator :: Config -> Validator
exampleValidator cfg = mkValidator cfg $ plam $ \_ _ _ -> popaque $ pconstant ()

exampleMintingPolicy :: Config -> MintingPolicy
exampleMintingPolicy cfg = mkMintingPolicy cfg $
  plam $ \_ _ -> popaque $ pconstant ()
