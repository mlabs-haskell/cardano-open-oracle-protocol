{ pkgs
, cardanoCli
, cardanoNode
, chainIndex
, coopPabCli
, coopPlutusCli
, coopPublisherCli
, jsFsStoreCli
, plutusJsonCli
, plutipLocalCluster
}:
pkgs.mkShell {
  packages = with pkgs; [
    jq
    sqlite
    protobuf
    protoc-gen-grpc-web
    haskellPackages.proto-lens-protoc
    nodePackages.npm
    nodejs
    grpcui
    grpcurl
    chainIndex
    cardanoCli
    cardanoNode
    coopPabCli
    coopPlutusCli
    coopPublisherCli
    jsFsStoreCli
    plutusJsonCli
    plutipLocalCluster
  ];
  shellHook = ''
    echo "Making proto and resources symlinks"
    rm -f coop-proto
    ln -s ${../../coop-proto} coop-proto
    rm -f resources
    ln -s ${../../coop-pab/resources} resources
    echo "Sourcing ${./aux.bash}"
    . ${./aux.bash}
    echo "Running on-load"
    on-load
    # WARN(bladyjoker): Running COOP services requires having $ export LC_CTYPE=C.UTF-8 LC_ALL=C.UTF-8 LANG=C.UTF-8
    echo "Exporting locale"
    export LC_CTYPE=C.UTF-8
    export LC_ALL=C.UTF-8
    export LANG=C.UTF-8
    export PS1='\[\e[0m\][\[\e[0;1;38;5;142m\]coop-env \[\e[0m\]~ \[\e[0m\]\W\[\e[0m\]] \[\e[0m\]\$ \[\e[0m\]'
    echo "Done"
  '';
}
