# TODO: Get rid of Plutarch
{ pkgs, haskell-nix, compiler-nix-name, plutarch, shellHook }:
let
  hn-extra-hackage = plutarch.inputs.haskell-nix-extra-hackage;
  myHackage = hn-extra-hackage.mkHackagesFor pkgs.system compiler-nix-name [
    "${plutarch.inputs.plutus}/plutus-tx"
  ];
in
haskell-nix.cabalProject' (plutarch.applyPlutarchDep pkgs rec {
  src = ./.;
  name = "coop-extras-plutus-json";
  inherit compiler-nix-name;
  inherit (myHackage) extra-hackages extra-hackage-tarballs;
  modules = myHackage.modules ++ [{
    packages = { };
  }];
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
      (plutarch.hlsFor compiler-nix-name pkgs.system)
    ];

    additional = ps: [
      ps.plutus-tx
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
