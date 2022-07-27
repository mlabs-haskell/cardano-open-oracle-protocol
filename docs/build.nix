{ pkgs, markdownlint-cli, shellHook }:
pkgs.mkShell {
  name = "docs-env";

  packages = [ markdownlint-cli ];

  shellHook = ''
    cd docs
    ${shellHook}
  '';
}
