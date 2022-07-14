{ src, proto, cabalPackageName, pkgs }:
let
  cabalTemplate = pkgs.writeTextFile {
    name = "protobuf-hs-cabal-template";
    text = ''
      cabal-version:      3.0
      name:               ${cabalPackageName}
      version:            0.1.0.0
      synopsis:           A Cabal project that contains protoc/proto-lens-protoc generated Haskell modules
      build-type:         Simple

      library
          exposed-modules: EXPOSED_MODULES
          autogen-modules: EXPOSED_MODULES

          hs-source-dirs:     src

          default-language: Haskell2010
          build-depends:
              base,
              proto-lens-runtime,
              proto-lens-protobuf-types
    '';
  };
in
pkgs.stdenv.mkDerivation {
  src = ./.;
  name = cabalPackageName;
  buildInputs = [
    pkgs.protobuf
    pkgs.haskellPackages.proto-lens-protoc
    pkgs.cabal-install
  ];

  buildPhase = ''
    set -vox
    mkdir src
    protoc --plugin=protoc-gen-haskell=${pkgs.haskellPackages.proto-lens-protoc}/bin/proto-lens-protoc \
           --proto_path=${src} \
           --haskell_out=src ${src}/${proto}

    EXPOSED_MODULES=$(find src -name "*.hs" | while read f; do grep -Eo 'module\s+\S+\s+' $f | sed -r 's/module\s+//' | sed -r 's/\s+//'; done | tr '\n' ' ')
    echo "Found generated modules $EXPOSED_MODULES"
    cat ${cabalTemplate} | sed -r "s/EXPOSED_MODULES/$EXPOSED_MODULES/" > ${cabalPackageName}.cabal
  '';

  installPhase = ''
    cp -r . $out
  '';
}
