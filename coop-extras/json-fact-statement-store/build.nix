{ pkgs, haskell-nix, compiler-nix-name, plutip, plutusJson, factStatementStoreProtoHs, cardanoProtoExtras, cardanoProtoHs, http2-grpc-native, shellHook }:
let
  proj = haskell-nix.cabalProject' {
    src = ./.;
    name = "coop-extras-json-fact-statement-store";
    inherit compiler-nix-name;
    index-state = "2022-05-16T00:00:00Z";
    inherit (plutip) cabalProjectLocal;
    modules = plutip.haskellModules ++ [
      {
        packages = {
          # Enable strict builds
          json-fact-statement-store.configureFlags = [ "-f-dev" ];

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
        src = http2-grpc-native;
        subdirs = [
          "http2-client-grpc"
          "http2-grpc-proto-lens"
          "http2-grpc-types"
          "warp-grpc"
        ];
      }
      {
        src = factStatementStoreProtoHs;
        subdirs = [ "." ];
      }
      {
        src = plutusJson;
        subdirs = [ "." ];
      }
      {
        src = cardanoProtoExtras;
        subdirs = [ "." ];
      }
      {
        src = cardanoProtoHs;
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
        grpcui
        grpcurl
      ];

      additional = ps: [
        ps.plutus-json
        ps.cardano-proto-extras
        ps.coop-cardano-proto

        # Needed to run the coop.TxBuilder gRpc service
        ps.http2-client-grpc
        ps.http2-grpc-proto-lens
        ps.http2-grpc-types
        ps.warp-grpc
        ps.coop-fact-statement-store-service-proto
      ];

      tools = {
        cabal = { };
        haskell-language-server = { };
      };

      shellHook = ''
        export LC_CTYPE=C.UTF-8
        export LC_ALL=C.UTF-8
        export LANG=C.UTF-8
        ${shellHook}
      '';

    };
  };
in
proj
