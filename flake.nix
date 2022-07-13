{
  description = "cardano-open-oracle-protocol";
  nixConfig.bash-prompt =
    "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]cardano-open-oracle-protocol \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";

  inputs = {
    # Plutip maintains a compatible Plutus/Cardano derivation set
    plutip.url = "github:mlabs-haskell/plutip";

    nixpkgs.follows = "plutip/nixpkgs";
    haskell-nix.follows = "plutip/haskell-nix";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    };

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    http2-grpc-native = {
      url = "github:bladyjoker/http2-grpc-haskell";
      flake = false;
    };

    plutarch.url = "github:Plutonomicon/plutarch-plutus";
    plutarch.inputs.haskell-nix.follows = "haskell-nix";
    plutarch.inputs.nixpkgs.follows = "nixpkgs";

    iohk-nix.follows = "plutip/iohk-nix";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , haskell-nix
    , pre-commit-hooks
    , http2-grpc-native
    , plutarch
    , iohk-nix
    , ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      inherit self;# To appease nix-linter

      pkgs = import nixpkgs {
        inherit system;
      };
      pkgsWithOverlay = import nixpkgs {
        inherit system;
        inherit (haskell-nix) config;
        overlays = [
          haskell-nix.overlay
          (import "${iohk-nix}/overlays/crypto")
        ];
      };

      # cardanoInputs = plutip.inputs.bot-plutus-interface.inputs;
      # cardanoExtraSources = plutip.inputs.bot-plutus-interface.extraSources;

      pre-commit-check = pre-commit-hooks.lib.${system}.run (import ./pre-commit-check.nix);
      pre-commit-devShell = pkgs.mkShell {
        inherit (pre-commit-check) shellHook;
      };

      oraclePureProj = import ./oracle-pure/build.nix {
        inherit pkgs;
        inherit (pkgsWithOverlay) haskell-nix;
        inherit (pre-commit-check) shellHook;
        compiler-nix-name = "ghc8107";
      };
      oraclePureFlake = oraclePureProj.flake { };

      protoHsProj = import ./proto/haskell/build.nix {
        inherit pkgs http2-grpc-native;
        inherit (pkgsWithOverlay) haskell-nix;
        inherit (pre-commit-check) shellHook;
        compiler-nix-name = "ghc8107";
      };
      protoHsFlake = protoHsProj.flake { };

      oraclePlutusProj = import ./oracle-plutus/build.nix {
        inherit pkgs plutarch;
        plutarchHsModule = plutarch.haskellModule system;
        inherit (pkgsWithOverlay) haskell-nix;
        inherit (pre-commit-check) shellHook;
        compiler-nix-name = "ghc921";
      };
      oraclePlutusFlake = oraclePlutusProj.flake { };

    in
    rec {

      # Standard flake attributes
      packages = oraclePureFlake.packages // protoHsFlake.packages // oraclePlutusFlake.packages;
      checks = oraclePureFlake.checks // protoHsFlake.checks // oraclePlutusFlake.checks // pre-commit-check;
      devShells = rec {
        proto = protoHsFlake.devShell;
        oracle-pure = oraclePureFlake.devShell;
        pre-commit = pre-commit-devShell;
        oracle-plutus = oraclePlutusFlake.devShell;
        default = proto;
      };

      # Used by CI
      build-all = pkgs.runCommand "build-all"
        (self.packages.${system} // self.devShells.${system})
        "touch $out";

      check-all = pkgs.runCommand "check-all"
        {
          nativeBuildInputs = builtins.attrValues self.checks.${system};
        } "touch $out";

      hydraJobs = {
        inherit build-all check-all;
      };

    });
}
