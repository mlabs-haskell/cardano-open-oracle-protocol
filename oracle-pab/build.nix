{ pkgs, haskell-nix, compiler-nix-name, plutip, oraclePlutusCli, oracle-hs-types, shellHook }:
let
  # FIXME: Use idiomatic cardano-node from bpi input
  cardanoNode = proj.hsPkgs.cardano-node.components.exes.cardano-node;
  cardanoCli = proj.hsPkgs.cardano-cli.components.exes.cardano-cli;
  proj = haskell-nix.cabalProject' {
    src = ./.;
    name = "oracle-pab";
    inherit compiler-nix-name;
    index-state = "2022-05-16T00:00:00Z";
    inherit (plutip) cabalProjectLocal;
    modules = plutip.haskellModules ++ [
      {
        packages = {
          # Enable strict builds
          oracle-pab.configureFlags = [ "-f-dev" ];

          # Link oracle-plutus-cli into tests
          oracle-pab.components.tests.oracle-pab-test.build-tools = [
            oraclePlutusCli
            cardanoNode
            cardanoCli
          ];

          # Don't use the new-ledger-namespace
          oracle-hs-types.configureFlags = [ "-f-new-ledger-namespace" ];
        };
      }
    ];

    extraSources = plutip.extraSources ++ [
      {
        src = plutip;
        subdirs = [ "." ];
      }
      {
        src = oracle-hs-types;
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
        oraclePlutusCli
        cardanoNode
        cardanoCli
      ];

      additional = ps: [
        ps.bot-plutus-interface
        ps.plutip
        ps.oracle-hs-types
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
