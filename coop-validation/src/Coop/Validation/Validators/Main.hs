module Coop.Validation.Validators.Main (mkValidator) where

import Plutarch (perror, plet, (:-->))
import Plutarch.Api.V2 (PValidator)
import Plutarch.Prelude (ClosedTerm, PAsData, PString, phoistAcyclic, plam, ptrace)
import Prelude (($))

-- NOTE(bladyjoker): Ask what's a safe way of including a `label` such that it remains a part of the script (and its hash) for maintaining script uniqueness.
mkValidator :: ClosedTerm (PAsData PString :--> PValidator)
mkValidator = phoistAcyclic $
  plam $
    \label _dat _rmdr _ctx -> ptrace "[@Main validator]" $ plet label (\_label -> ptrace "@Main validator supports no actions" perror)
