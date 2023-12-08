{ inputs, ... }:
{
  perSystem = { config, system, pkgs, inputs', ... }:
    let
      hsFlake = inputs.lbf.lib.${system}.haskellPlutusFlake {
        src = ./.;

        name = "coop-validation";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
          # LB base schema and runtimes libs
          # Plutarch
          "${inputs'.lbf.packages.lbf-prelude-plutarch}"
          "${inputs'.lbf.packages.lbf-plutus-plutarch}"
          "${inputs'.lbf.packages.lbr-plutarch-src}"
          # Haskell Prelude
          "${inputs'.lbf.packages.lbf-prelude-haskell}"
          "${inputs'.lbf.packages.lbr-prelude-haskell-src}"
          # Haskell Plutus
          "${inputs'.lbf.packages.lbf-plutus-haskell}"
          "${inputs'.lbf.packages.lbr-plutus-haskell-src}"

          # Plutarch itself
          "${inputs.plutarch}"
          "${inputs.plutarch}/plutarch-extra"
          "${inputs.plutarch}/plutarch-test"

          # Coop API
          "${config.packages.lbf-coop-plutus-api-plutarch}"
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };

    in
    {
      devShells.dev-coop-validation = hsFlake.devShell;

      packages = {
        # WARN(bladyjoker): We have to pick the hsFlake.packages like this otherwise flake-parts goes into `infinite recursion`.
        coop-validation-lib = hsFlake.packages."coop-validation:lib:coop-validation";

        coop-validation-cli = hsFlake.packages."coop-validation:exe:coop-validation-cli";

        coop-validation-config = pkgs.stdenv.mkDerivation {
          name = "coop-validation-config";
          src = ./.;
          buildPhase = ''${config.packages.coop-validation-cli}/bin/coop-validation-cli compile'';
          installPhase = "cp coop-config.json $out";
        };
      };

      inherit (hsFlake) checks;
    };
}
