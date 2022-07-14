module Cardano.Oracle.Plutus (exampleFunction, exampleValidator) where

import Plutarch (popaque)
import Plutarch.Api.V1 (mkValidator)
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
import Plutus.V1.Ledger.Scripts (Validator)

pletC :: Term s a -> TermCont s (Term s a)
pletC = tcont . plet

exampleFunction :: Term s (PInteger :--> PInteger)
exampleFunction = plam $ \x -> ptrace "exampleFunctions" $
  unTermCont $ do
    x' <- pletC $ x + 1 + 2 + 3
    return $ x' + x'

exampleValidator :: Validator
exampleValidator = mkValidator $
  plam $ \_ _ _ -> popaque $ pconstant ()
