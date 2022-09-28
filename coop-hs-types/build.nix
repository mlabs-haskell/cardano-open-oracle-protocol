{ pkgs, haskell-nix, compiler-nix-name, plutip, shellHook }:
haskell-nix.cabalProject' {
  src = ./.;
  name = "coop-hs-types";
  inherit compiler-nix-name;
  index-state = "2022-05-16T00:00:00Z";
  inherit (plutip) cabalProjectLocal;
  modules = plutip.haskellModules ++ [{
    packages = {
      # Enable strict builds
      oracle-hs-types.configureFlags = [ "-f-dev" ];
    };
  }];

  extraSources = plutip.extraSources;

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

    additional = ps: [ ps.plutus-ledger-api ps.plutus-tx ps.serialise ];

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
}
