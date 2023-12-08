{
  description = "cardano-open-oracle-protocol";

  inputs = {
    lbf.url = "github:mlabs-haskell/lambda-buffers";
    haskell-nix.follows = "lbf/haskell-nix";
    nixpkgs.follows = "lbf/nixpkgs";
    pre-commit-hooks.follows = "lbf/pre-commit-hooks";
    hci-effects.follows = "lbf/hci-effects";
    ctl.follows = "lbf/ctl";
    iohk-nix.follows = "lbf/iohk-nix";
    flake-parts.follows = "lbf/flake-parts";
    plutarch.follows = "lbf/plutarch";

    # Plutip maintains a compatible Plutus/Cardano derivation set
    bot-plutus-interface.url = "github:mlabs-haskell/bot-plutus-interface";

    plutip.url = "github:mlabs-haskell/plutip";
    plutip.inputs.bot-plutus-interface.follows = "bot-plutus-interface";
    plutip.inputs.haskell-nix.follows = "bot-plutus-interface/haskell-nix";
    plutip.inputs.iohk-nix.follows = "bot-plutus-interface/iohk-nix";
    plutip.inputs.nixpkgs.follows = "bot-plutus-interface/nixpkgs";


    flake-utils.url = "github:numtide/flake-utils";

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    # TODO: Merge with upstream and use that.
    http2-grpc-native = {
      url = "github:bladyjoker/http2-grpc-haskell";
      flake = false;
    };

    custom-config.url = "github:input-output-hk/empty-flake";
  };
  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./pkgs.nix
        ./settings.nix
        ./pre-commit.nix
        ./hercules-ci.nix
        ./coop-api/lbf/build.nix
        ./coop-validation/build.nix
      ];
      debug = true;
      systems = [ "x86_64-linux" "x86_64-darwin" ];
    };
}
