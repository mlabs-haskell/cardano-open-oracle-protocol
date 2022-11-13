{ pkgs, haskell-nix, compiler-nix-name, plutip, coopPlutusCli, plutipLocalCluster, coop-hs-types, txBuilderProtoHs, cardanoProtoHs, cardanoProtoExtras, http2-grpc-native, shellHook }:
let
  # FIXME: Use idiomatic cardano-node from bpi input
  cardanoNode = proj.hsPkgs.cardano-node.components.exes.cardano-node;
  cardanoCli = proj.hsPkgs.cardano-cli.components.exes.cardano-cli;
  proj = haskell-nix.cabalProject' {
    src = ./.;
    name = "coop-pab";
    inherit compiler-nix-name;
    index-state = "2022-05-16T00:00:00Z";
    inherit (plutip) cabalProjectLocal;
    modules = plutip.haskellModules ++ [
      {
        packages = {
          # Enable strict builds
          coop-pab.configureFlags = [ "-f-dev" ];

          # Link coop-plutus-cli into tests
          coop-pab.components.tests.coop-pab-tests.build-tools = [
            coopPlutusCli
            cardanoNode
            cardanoCli
          ];

          # Don't use the new-ledger-namespace
          coop-hs-types.configureFlags = [ "-f-new-ledger-namespace" ];

          # FIXME: This is annoying
          # Add proto compilation execs
          proto-lens-protobuf-types.components.library.build-tools = [
            pkgs.protobuf
            pkgs.haskellPackages.proto-lens-protoc
          ];

        };
      }
    ];

    extraSources = plutip.extraSources ++ [
      {
        src = plutip;
        subdirs = [ "." ];
      }
      {
        src = coop-hs-types;
        subdirs = [ "." ];
      }
      {
        src = http2-grpc-native;
        subdirs = [
          "http2-client-grpc"
          "http2-grpc-proto-lens"
          "http2-grpc-types"
          "warp-grpc"
        ];
      }
      {
        src = txBuilderProtoHs;
        subdirs = [ "." ];
      }
      {
        src = cardanoProtoHs;
        subdirs = [ "." ];
      }
      {
        src = cardanoProtoExtras;
        subdirs = [ "." ];
      }
    ];

    shell = {
      withHoogle = true;

      exactDeps = true;

      nativeBuildInputs = with pkgs; [
        # Code quality
        ## Haskell/Cabal
        haskellPackages.apply-refact
        haskellPackages.fourmolu
        haskellPackages.cabal-fmt
        hlint
        coopPlutusCli
        cardanoNode
        cardanoCli
        grpcui
        grpcurl

        plutipLocalCluster
      ];

      additional = ps: [
        ps.bot-plutus-interface
        ps.plutip
        ps.coop-hs-types
        ps.cardano-proto-extras
        ps.coop-cardano-proto

        # Needed to run the coop.TxBuilder gRpc service
        ps.http2-client-grpc
        ps.http2-grpc-proto-lens
        ps.http2-grpc-types
        ps.warp-grpc
        ps.coop-tx-builder-service-proto
      ];

      tools = {
        cabal = { };
        haskell-language-server = { };
      };

      shellHook = ''
        export LC_CTYPE=C.UTF-8
        export LC_ALL=C.UTF-8
        export LANG=C.UTF-8
        source ${./aux.sh}
        ${shellHook}
      '';

    };
  };
in
proj
