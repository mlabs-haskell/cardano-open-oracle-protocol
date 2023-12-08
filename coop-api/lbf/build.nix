{ inputs, ... }: {
  perSystem = { system, ... }:
    {
      packages.lbf-coop-plutus-api-haskell = inputs.lbf.lib."${system}".lbfPlutusHaskell {
        name = "lbf-coop-plutus-api";
        src = ./.;
        files = [ "Coop/Validation/Plutus.lbf" ];
      };

      packages.lbf-coop-plutus-api-purescript = inputs.lbf.lib."${system}".lbfPlutusPurescript {
        name = "lbf-coop-plutus-api";
        src = ./.;
        files = [ "Coop/Validation/Plutus.lbf" ];
      };

      packages.lbf-coop-plutus-api-plutarch = inputs.lbf.lib."${system}".lbfPlutarch {
        name = "lbf-coop-plutus-api-plutarch";
        src = ./.;
        files = [ "Coop/Validation/Plutus.lbf" ];
      };

    };
}
