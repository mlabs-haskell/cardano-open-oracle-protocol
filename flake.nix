{
  description = "cardano-open-oracle-protocol";
  nixConfig.bash-prompt =
    "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]cardano-open-oracle-protocol \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";

  inputs = {
    # Plutip maintains a compatible Plutus/Cardano derivation set
    plutip.url = "github:mlabs-haskell/plutip/gergely/vasil-with-latest-wallet";

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

    plutarch.url = "github:Plutonomicon/plutarch-plutus/staging";

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
    , plutip
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
      let
        inherit self;

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

        protoDevShell = import ./proto/build.nix {
          inherit pkgs;
          inherit (pre-commit-check) shellHook;
        };

        oracleHsTypesProj = import ./oracle-hs-types/build.nix {
          inherit pkgs plutip;
          inherit (pkgsWithOverlay) haskell-nix;
          inherit (pre-commit-check) shellHook;
          compiler-nix-name = "ghc8107";
        };
        oracleHsTypesFlake = oracleHsTypesProj.flake { };

        pkgsForPlutarch = import plutarch.inputs.nixpkgs {
          inherit system;
          inherit (plutarch.inputs.haskell-nix) config;
          overlays = [
            plutarch.inputs.haskell-nix.overlay
            (import "${plutarch.inputs.iohk-nix}/overlays/crypto")
          ];
        };

        oraclePlutusProj = import ./oracle-plutus/build.nix {
          inherit plutarch;
          pkgs = pkgsForPlutarch;
          inherit (pkgsForPlutarch) haskell-nix;
          inherit (pre-commit-check) shellHook;
          oracle-hs-types = ./oracle-hs-types;
          compiler-nix-name = "ghc923";
        };
        oraclePlutusFlake = oraclePlutusProj.flake { };

        oracleHsProto = import ./nix/protobuf-hs.nix {
          inherit pkgs;
          src = ./proto;
          proto = "oracle.proto";
          cabalPackageName = "oracle-proto";
        };

        oracleServiceProj = import ./oracle-service/build.nix {
          inherit pkgs http2-grpc-native;
          oracle-proto = oracleHsProto;
          inherit (pkgsWithOverlay) haskell-nix;
          inherit (pre-commit-check) shellHook;
          compiler-nix-name = "ghc8107";
        };
        oracleServiceFlake = oracleServiceProj.flake { };

        docsDevShell = import ./docs/build.nix {
          inherit pkgs;
          inherit (pre-commit-hooks.outputs.packages.${system}) markdownlint-cli;
          inherit (pre-commit-check) shellHook;
        };

        oraclePabProj = import ./oracle-pab/build.nix {
          inherit pkgs plutip;
          inherit (pkgsWithOverlay) haskell-nix;
          inherit (pre-commit-check) shellHook;
          oracle-hs-types = ./oracle-hs-types;
          compiler-nix-name = "ghc8107";
        };
        oraclePabFlake = oraclePabProj.flake { };
      in
      rec {
        # Useful for nix repl
        inherit pkgs pkgsWithOverlay pkgsForPlutarch;

        # Standard flake attributes
        packages = oraclePureFlake.packages // oraclePlutusFlake.packages // oracleServiceFlake.packages // oraclePabFlake.packages // oracleHsTypesFlake.packages;

        devShells = rec {
          dev-proto = protoDevShell;
          dev-pure = oraclePureFlake.devShell;
          dev-pre-commit = pre-commit-devShell;
          dev-plutus = oraclePlutusFlake.devShell;
          dev-service = oracleServiceFlake.devShell;
          dev-docs = docsDevShell;
          dev-pab = oraclePabFlake.devShell;
          dev-hs-types = oracleHsTypesFlake.devShell;
          default = dev-proto;
        };

        checks = oraclePureFlake.checks //
          oraclePlutusFlake.checks //
          oracleServiceFlake.checks //
          oraclePabFlake.checks //
          oracleHsTypesFlake.checks //
          { inherit pre-commit-check; } // devShells // packages;
      });
}
