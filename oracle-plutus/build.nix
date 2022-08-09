{ pkgs, haskell-nix, compiler-nix-name, oracle-hs-types, plutarch, shellHook }:
let
  hn-extra-hackage = plutarch.inputs.haskell-nix-extra-hackage;
  myHackage = hn-extra-hackage.mkHackagesFor pkgs.system compiler-nix-name [
    "${plutarch}"
    "${plutarch}/plutarch-extra"
    "${plutarch}/plutarch-test"
    "${plutarch.inputs.plutus}/plutus-ledger-api"
    "${oracle-hs-types}"
  ];
in
haskell-nix.cabalProject' (plutarch.applyPlutarchDep pkgs rec {
  src = ./.;
  name = "oracle-plutus";
  inherit compiler-nix-name;
  inherit (myHackage) extra-hackages extra-hackage-tarballs;
  modules = myHackage.modules ++ [{
    packages = {
      # Enable strict builds
      oracle-plutus.configureFlags = [ "-f-dev" ];

      # Use the new-ledger-namespace
      oracle-hs-types.configureFlags = [ "-fnew-ledger-namespace" ];
    };
  }];
  shell = {
    # FIXME: withHoogle = true doesn't work
    withHoogle = false;

    exactDeps = true;

    nativeBuildInputs = with pkgs; [
      # Code quality
      ## Haskell/Cabal
      haskellPackages.apply-refact
      haskellPackages.fourmolu
      haskellPackages.cabal-fmt
      hlint
      (plutarch.hlsFor compiler-nix-name pkgs.system)
    ];

    additional = ps: [
      ps.plutarch
      ps.plutarch-extra
      ps.plutarch-test
      ps.plutus-ledger-api
      ps.oracle-hs-types
    ];

    tools = {
      cabal = { };
    };
    shellHook = ''
      export LC_CTYPE=C.UTF-8
      export LC_ALL=C.UTF-8
      export LANG=C.UTF-8
      ${shellHook}
    '';

  };
})
