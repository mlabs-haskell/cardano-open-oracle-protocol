{ pkgs, haskell-nix, compiler-nix-name, http2-grpc-native, cardanoProtoHs, publisherProtoHs, txBuilderProtoHs, factStatementStoreProtoHs, shellHook }:
haskell-nix.cabalProject' {
  src = ./.;
  name = "coop-publisher";
  inherit compiler-nix-name;
  index-state = "2022-01-21T23:44:46Z";
  extraSources = [
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
      src = cardanoProtoHs;
      subdirs = [ "." ];
    }
    {
      src = publisherProtoHs;
      subdirs = [ "." ];
    }
    {
      src = txBuilderProtoHs;
      subdirs = [ "." ];
    }
    {
      src = factStatementStoreProtoHs;
      subdirs = [ "." ];
    }

  ];
  modules = [
    (_: {
      packages = {
        allComponent.doHoogle = true;
        allComponent.doHaddock = true;

        # FIXME: This is annoying
        # Add proto compilation execs
        proto-lens-protobuf-types.components.library.build-tools = [
          pkgs.protobuf
          pkgs.haskellPackages.proto-lens-protoc
        ];

      };
    })
  ];
  shell = {

    withHoogle = true;

    exactDeps = true;

    # We use the ones from vanilla Nixpkgs, since they are cached reliably.
    nativeBuildInputs = with pkgs; [
      # Code quality
      ## Haskell/Cabal
      haskellPackages.fourmolu
      haskellPackages.cabal-fmt
      hlint
      ## Nix
      nixpkgs-fmt
      grpcui
      grpcurl
    ];

    additional = ps: [
      ps.http2-client-grpc
      ps.http2-grpc-proto-lens
      ps.http2-grpc-types
      ps.warp-grpc
      ps.coop-cardano-proto
      ps.coop-publisher-service-proto
      ps.coop-tx-builder-service-proto
      ps.coop-fact-statement-store-service-proto
    ];

    tools = {
      cabal = { };
      hlint = { };
      haskell-language-server = { };
    };

    shellHook = ''
      export LC_CTYPE=C.UTF-8
      export LC_ALL=C.UTF-8
      export LANG=C.UTF-8
      ${shellHook}
    '';
  };
}
