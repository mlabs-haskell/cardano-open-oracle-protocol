{ pkgs, haskell-nix, compiler-nix-name, shellHook }:
haskell-nix.project' {
  inherit compiler-nix-name;
  src = ./.;
  shell = {
    buildInputs =
      [
        pkgs.nixpkgs-fmt
        pkgs.haskellPackages.cabal-fmt
        pkgs.haskellPackages.fourmolu
      ];
    tools = {
      cabal = { };
      hlint = { };
      haskell-language-server = { };
    };
    shellHook = ''
      ${shellHook}
    '';
  };
}
