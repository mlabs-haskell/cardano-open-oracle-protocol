{ pkgs, haskell-nix, compiler-nix-name, plutip, cardanoProtoHs, shellHook }:
let
  proj = haskell-nix.cabalProject' {
    src = ./.;
    name = "cardano-proto-extras";
    inherit compiler-nix-name;
    index-state = "2022-05-16T00:00:00Z";
    inherit (plutip) cabalProjectLocal;
    modules = plutip.haskellModules ++ [
      {
        packages = {
          # Enable strict builds
          cardano-protobuf-extras.configureFlags = [ "-f-dev" ];

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
      ];

      additional = ps: [
        ps.coop-cardano-proto
        ps.plutus-tx
        ps.plutus-ledger-api
        ps.plutus-ledger
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
