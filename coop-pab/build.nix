{ pkgs, haskell-nix, compiler-nix-name, plutip, coopPlutusCli, coop-hs-types, shellHook }:
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
          coop-pab.configureFlags = [ "-f-dev" ];

          # Link coop-plutus-cli into tests
          coop-pab.components.tests.coop-pab-tests.build-tools = [
            coopPlutusCli
            cardanoNode
            cardanoCli
          ];

          # Don't use the new-ledger-namespace
          coop-hs-types.configureFlags = [ "-f-new-ledger-namespace" ];
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
      ];

      additional = ps: [
        ps.bot-plutus-interface
        ps.plutip
        ps.coop-hs-types
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
