{ pkgs, haskell-nix, compiler-nix-name, http2-grpc-native, shellHook }:
haskell-nix.cabalProject' {
  src = ./.;
  name = "cardano-oracle-proto";
  inherit compiler-nix-name;
  index-state = "2022-01-21T23:44:46Z";
  extraSources = [
    {
      src = http2-grpc-native;
      subdirs = [
        "http2-client-grpc"
        "http2-grpc-proto-lens"
        #"http2-grpc-proto3-wire"
        "http2-grpc-types"
        "warp-grpc"
      ];
    }
  ];
  modules = [
    (_: {
      packages = {
        allComponent.doHoogle = true;

        # Add proto compilation execs
        proto-lens-protobuf-types.components.library.build-tools = [
          pkgs.protobuf
          pkgs.haskellPackages.proto-lens-protoc
        ];

        cardano-oracle-proto.components.library.build-tools = [
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
      # Building code
      protobuf
      protoc-gen-grpc-web
      haskellPackages.proto-lens-protoc
      nodePackages.npm
      nodejs
      # Code quality
      ## Haskell/Cabal
      haskellPackages.fourmolu
      haskellPackages.cabal-fmt
      hlint
      ## Nix
      nixpkgs-fmt
    ];

    additional = ps: [
      ps.http2-client-grpc
      ps.http2-grpc-proto-lens
      #ps.http2-grpc-proto3-wire
      ps.http2-grpc-types
      ps.warp-grpc
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
      cd proto
      ${shellHook}
    '';
  };
}
