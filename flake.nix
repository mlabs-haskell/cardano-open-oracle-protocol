{
  description = "cardano-open-oracle-protocol";

  inputs = {
    # Plutip maintains a compatible Plutus/Cardano derivation set
    # TODO: Set to a stable release
    bot-plutus-interface.url = "github:mlabs-haskell/bot-plutus-interface/sam/add-vasil-features";

    # TODO: Set to a stable release
    plutip.url = "github:mlabs-haskell/plutip/bladyjoker/upgrade-to-sam-vasil";
    plutip.inputs.bot-plutus-interface.follows = "bot-plutus-interface";
    plutip.inputs.haskell-nix.follows = "bot-plutus-interface/haskell-nix";
    plutip.inputs.iohk-nix.follows = "bot-plutus-interface/iohk-nix";
    plutip.inputs.nixpkgs.follows = "bot-plutus-interface/nixpkgs";

    nixpkgs.follows = "plutip/nixpkgs";
    haskell-nix.follows = "plutip/haskell-nix";


    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "haskell-nix/nixpkgs";
    };

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    # TODO: Merge with upstream and use that.
    http2-grpc-native = {
      url = "github:bladyjoker/http2-grpc-haskell";
      flake = false;
    };

    plutarch.url = "github:plutonomicon/plutarch-plutus/c32001b2ae3007572cb6d5256072a2529c1a3407";
    plutarch.inputs.nixpkgs.follows = "nixpkgs";

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
    , ...
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
        fourmolu = pkgsFourmolu.haskell.packages.ghc924.fourmolu;
        pre-commit-check = pre-commit-hooks.lib.${system}.run (import ./pre-commit-check.nix { inherit fourmolu; });
        pre-commit-devShell = pkgs.mkShell {
          inherit (pre-commit-check) shellHook;
        };

        # Haskell shared types
        coopHsTypesProj = import ./coop-hs-types/build.nix {
          inherit pkgs plutip;
          inherit (pkgsWithOverlay) haskell-nix;
          inherit (pre-commit-check) shellHook;
          compiler-nix-name = "ghc8107";
        };
        coopHsTypesFlake = coopHsTypesProj.flake { };

        # Plutus
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
        coopPlutusCli = coopPlutusProj.getComponent "coop-plutus:exe:coop-plutus-cli";

        # Publisher
        coopPublisherProj = import ./coop-publisher/build.nix {
          inherit pkgs http2-grpc-native;
          inherit (pkgsWithOverlay) haskell-nix;
          inherit (pre-commit-check) shellHook;
          inherit cardanoProtoHs txBuilderProtoHs factStatementStoreProtoHs publisherProtoHs;
          compiler-nix-name = "ghc8107";
        };
        coopPublisherFlake = coopPublisherProj.flake { };

        # Docs
        coopDocsDevShell = import ./coop-docs/build.nix {
          inherit pkgs;
          inherit (pre-commit-hooks.outputs.packages.${system}) markdownlint-cli;
          inherit (pre-commit-check) shellHook;
        };

        # Protos
        coopProtoDevShell = import ./coop-proto/build.nix {
          inherit pkgs;
          inherit (pre-commit-check) shellHook;
        };

        cardanoProtoHs = import ./nix/protobuf-hs.nix {
          inherit pkgs;
          src = ./coop-proto;
          proto = "cardano.proto";
          cabalPackageName = "coop-cardano-proto";
        };

        txBuilderProtoHs = import ./nix/protobuf-hs.nix {
          inherit pkgs;
          src = ./coop-proto;
          proto = "tx-builder-service.proto";
          buildDepends = [ "coop-cardano-proto" ];
          cabalPackageName = "coop-tx-builder-service-proto";
        };

        factStatementStoreProtoHs = import ./nix/protobuf-hs.nix {
          inherit pkgs;
          src = ./coop-proto;
          proto = "fact-statement-store-service.proto";
          buildDepends = [ "coop-cardano-proto" ];
          cabalPackageName = "coop-fact-statement-store-service-proto";
        };

        publisherProtoHs = import ./nix/protobuf-hs.nix {
          inherit pkgs;
          src = ./coop-proto;
          proto = "publisher-service.proto";
          buildDepends = [ "coop-cardano-proto" "coop-fact-statement-store-service-proto" "coop-tx-builder-service-proto" ];
          cabalPackageName = "coop-publisher-service-proto";
        };

        # PAB
        coopPabProj = import ./coop-pab/build.nix {
          inherit pkgs plutip coopPlutusCli http2-grpc-native;
          inherit (pkgsWithOverlay) haskell-nix;
          inherit (pre-commit-check) shellHook;
          coop-hs-types = ./coop-hs-types;
          cardanoProtoExtras = ./coop-proto/cardano-proto-extras;
          inherit cardanoProtoHs txBuilderProtoHs;
          plutipLocalCluster = plutip.packages.${system}."plutip:exe:local-cluster";
          compiler-nix-name = "ghc8107";
        };
        coopPabFlake = coopPabProj.flake { };

        coopPabCli = pkgs.stdenv.mkDerivation {
          name = "coop-pab-cli";
          nativeBuildInputs = [ pkgs.makeWrapper ];
          phases = [ "installPhase" ];
          installPhase = ''
            mkdir -p $out/bin
            cp ${coopPabProj.getComponent "coop-pab:exe:coop-pab-cli"}/bin/coop-pab-cli $out/bin/coop-pab-cli
            chmod +x $out/bin/coop-pab-cli
            wrapProgram  $out/bin/coop-pab-cli --prefix PATH ":" ${coopPlutusCli}/bin/coop-plutus-cli
          '';
        };

        # Extras
        coopExtrasPlutusJson = import ./coop-extras/plutus-json/build.nix {
          inherit plutarch;
          pkgs = pkgsForPlutarch;
          inherit (pkgsForPlutarch) haskell-nix;
          inherit (pre-commit-check) shellHook;
          compiler-nix-name = "ghc923";
        };
        coopExtrasPlutusJsonFlake = coopExtrasPlutusJson.flake { };

        coopExtrasJsonFactStatementStore = import ./coop-extras/json-fact-statement-store/build.nix {
          inherit pkgs plutip http2-grpc-native;
          inherit (pkgsWithOverlay) haskell-nix;
          inherit (pre-commit-check) shellHook;
          inherit factStatementStoreProtoHs cardanoProtoHs;
          cardanoProtoExtras = ./coop-proto/cardano-proto-extras;
          plutusJson = ./coop-extras/plutus-json;
          compiler-nix-name = "ghc8107";
        };
        coopExtrasJsonFactStatementStoreFlake = coopExtrasJsonFactStatementStore.flake { };

        cardanoProtoExtras = import ./coop-proto/cardano-proto-extras/build.nix {
          inherit pkgs plutip;
          inherit (pkgsWithOverlay) haskell-nix;
          inherit (pre-commit-check) shellHook;
          inherit cardanoProtoHs;
          compiler-nix-name = "ghc8107";
        };
        cardanoProtoExtrasFlake = cardanoProtoExtras.flake { };

        coopEnvShell = import ./coop-extras/coop-env/build.nix {
          inherit pkgs;
          plutipLocalCluster = plutip.packages.${system}."plutip:exe:local-cluster";
          jsFsStoreCli = coopExtrasJsonFactStatementStoreFlake.packages."json-fact-statement-store:exe:json-fs-store-cli";
          inherit coopPabCli coopPlutusCli;
          coopPublisherCli = coopPublisherFlake.packages."coop-publisher:exe:coop-publisher-cli";
          cardanoNode = coopPabProj.hsPkgs.cardano-node.components.exes.cardano-node;
          cardanoCli = coopPabProj.hsPkgs.cardano-cli.components.exes.cardano-cli;
        };

        renameAttrs = rnFn: pkgs.lib.attrsets.mapAttrs' (n: value: { name = rnFn n; inherit value; });
      in
      rec {
        # Useful for nix repl
        inherit pkgs pkgsWithOverlay pkgsForPlutarch;

        # Standard flake attributes
        packages = coopPlutusFlake.packages // coopPublisherFlake.packages // coopPabFlake.packages // coopHsTypesFlake.packages // {
          "coop-pab-cli" = coopPabCli;
        };

        devShells = rec {
          dev-proto = coopProtoDevShell;
          dev-pre-commit = pre-commit-devShell;
          dev-plutus = coopPlutusFlake.devShell;
          dev-service = coopPublisherFlake.devShell;
          dev-docs = coopDocsDevShell;
          dev-pab = coopPabFlake.devShell;
          dev-hs-types = coopHsTypesFlake.devShell;
          dev-extras-plutus-json = coopExtrasPlutusJsonFlake.devShell;
          dev-extras-json-store = coopExtrasJsonFactStatementStoreFlake.devShell;
          coop-env = coopEnvShell;
          dev-cardano-proto-extras = cardanoProtoExtrasFlake.devShell;
          default = dev-proto;
        };

        checks = renameAttrs (n: "check-${n}")
          (coopPlutusFlake.checks //
            coopPublisherFlake.checks //
            coopPabFlake.checks //
            coopHsTypesFlake.checks //
            coopExtrasPlutusJsonFlake.checks //
            coopExtrasJsonFactStatementStoreFlake.checks //
            cardanoProtoExtrasFlake.checks
          ) //
        { inherit pre-commit-check; } // devShells // packages;
      });
}
