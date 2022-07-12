{ pkgs, haskell-nix, compiler-nix-name, http2-grpc-native, shellHook }:
haskell-nix.cabalProject' {
  src = ./.;
  name = "orcfax-proto";
  inherit compiler-nix-name;
  index-state = "2022-01-21T23:44:46Z";
  extraSources = [
    {
      src = http2-grpc-native + /http2-client-grpc;
      subdirs = [
        "."
      ];
    }
    {
      src = http2-grpc-native + /http2-grpc-proto-lens;
      subdirs = [
        "."
      ];
    }
    {
      src = http2-grpc-native + /http2-grpc-proto3-wire;
      subdirs = [
        "."
      ];
    }
    {
      src = http2-grpc-native + /http2-grpc-types;
      subdirs = [
        "."
      ];
    }

    {
      src = http2-grpc-native + /warp-grpc;
      subdirs = [
        "."
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

        orcfax-proto.components.library.build-tools = [
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
      haskellPackages.proto-lens-protoc
      # Code quality
      ## Haskell/Cabal
      haskellPackages.fourmolu
      haskellPackages.cabal-fmt
      ## Nix
      nixpkgs-fmt
    ];

    # additional = ps: [
    #   ps.http2-client-grpc
    #   ps.http2-grpc-proto-lens
    #   ps.http2-grpc-proto3-wire
    #   ps.http2-grpc-types
    #   ps.warp-grpc
    # ];

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
