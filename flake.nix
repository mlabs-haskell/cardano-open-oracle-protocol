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

    nixpkgs-fourmolu.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

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
    , nixpkgs-fourmolu
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
        pkgsFourmolu = import nixpkgs-fourmolu {
          inherit system;
        };
        fourmolu = pkgsFourmolu.haskell.packages.ghc924.fourmolu_0_6_0_0;
        pre-commit-check = pre-commit-hooks.lib.${system}.run (import ./pre-commit-check.nix { inherit fourmolu; });
        pre-commit-devShell = pkgs.mkShell {
          inherit (pre-commit-check) shellHook;
        };

        coopPureProj = import ./coop-pure/build.nix {
          inherit pkgs;
          inherit (pkgsWithOverlay) haskell-nix;
          inherit (pre-commit-check) shellHook;
          compiler-nix-name = "ghc8107";
        };
        coopPureFlake = coopPureProj.flake { };

        coopProtoDevShell = import ./coop-proto/build.nix {
          inherit pkgs;
          inherit (pre-commit-check) shellHook;
        };

        coopHsTypesProj = import ./coop-hs-types/build.nix {
          inherit pkgs plutip;
          inherit (pkgsWithOverlay) haskell-nix;
          inherit (pre-commit-check) shellHook;
          compiler-nix-name = "ghc8107";
        };
        coopHsTypesFlake = coopHsTypesProj.flake { };

        pkgsForPlutarch = import plutarch.inputs.nixpkgs {
          inherit system;
          inherit (plutarch.inputs.haskell-nix) config;
          overlays = [
            plutarch.inputs.haskell-nix.overlay
            (import "${plutarch.inputs.iohk-nix}/overlays/crypto")
          ];
        };

        coopPlutusProj = import ./coop-plutus/build.nix {
          inherit plutarch;
          pkgs = pkgsForPlutarch;
          inherit (pkgsForPlutarch) haskell-nix;
          inherit (pre-commit-check) shellHook;
          coop-hs-types = ./coop-hs-types;
          compiler-nix-name = "ghc923";
        };
        coopPlutusFlake = coopPlutusProj.flake { };

        coopHsProto = import ./nix/protobuf-hs.nix {
          inherit pkgs;
          src = ./coop-proto;
          proto = "coop.proto";
          cabalPackageName = "coop-proto";
        };

        coopPublisherProj = import ./coop-publisher/build.nix {
          inherit pkgs http2-grpc-native coopHsProto;
          inherit (pkgsWithOverlay) haskell-nix;
          inherit (pre-commit-check) shellHook;
          compiler-nix-name = "ghc8107";
        };
        coopPublisherFlake = coopPublisherProj.flake { };

        coopDocsDevShell = import ./coop-docs/build.nix {
          inherit pkgs;
          inherit (pre-commit-hooks.outputs.packages.${system}) markdownlint-cli;
          inherit (pre-commit-check) shellHook;
        };

        coopPabProj = import ./coop-pab/build.nix {
          inherit pkgs plutip;
          inherit (pkgsWithOverlay) haskell-nix;
          inherit (pre-commit-check) shellHook;
          coopPlutusCli = coopPlutusProj.getComponent "coop-plutus:exe:coop-plutus-cli";
          coop-hs-types = ./coop-hs-types;
          compiler-nix-name = "ghc8107";
        };
        coopPabFlake = coopPabProj.flake { };

        renameAttrs = rnFn: pkgs.lib.attrsets.mapAttrs' (n: value: { name = rnFn n; inherit value; });
      in
      rec {
        # Useful for nix repl
        inherit pkgs pkgsWithOverlay pkgsForPlutarch;

        # Standard flake attributes
        packages = coopPureFlake.packages // coopPlutusFlake.packages // coopPublisherFlake.packages // coopPabFlake.packages // coopHsTypesFlake.packages;

        devShells = rec {
          dev-proto = coopProtoDevShell;
          dev-pure = coopPureFlake.devShell;
          dev-pre-commit = pre-commit-devShell;
          dev-plutus = coopPlutusFlake.devShell;
          dev-service = coopPublisherFlake.devShell;
          dev-docs = coopDocsDevShell;
          dev-pab = coopPabFlake.devShell;
          dev-hs-types = coopHsTypesFlake.devShell;
          default = dev-proto;
        };

        checks = renameAttrs (n: "check-${n}")
          (coopPureFlake.checks //
            coopPlutusFlake.checks //
            coopPublisherFlake.checks //
            coopPabFlake.checks //
            coopHsTypesFlake.checks) //
        { inherit pre-commit-check; } // devShells // packages;
      });
}
