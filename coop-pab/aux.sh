function generate-keys {
    local WORKDIR=.coop-pab-cli
    local RESOURCES=resources
    openssl genrsa -out $WORKDIR/key.pem 2048
    openssl req -new -key $WORKDIR/key.pem -out $WORKDIR/certificate.csr
    openssl x509 -req -in $WORKDIR/certificate.csr -signkey $WORKDIR/key.pem -out $WORKDIR/certificate.pem -extfile $RESOURCES/ssl-extensions-x509.conf -extensions v3_ca
    openssl x509 -text -in $WORKDIR/certificate.pem
}

function start-cluster {
    # As specified in resources/pabConfig.yaml
    rm -fr .wallets
    rm -fr .local-cluster
    mkdir .wallet
    mkdir .local-cluster
    mkdir .local-cluster/txs
    mkdir .local-cluster/scripts
    local-cluster --wallet-dir .wallets -n 10 --utxos 5 --chain-index-port 9084 --slot-len 1s --epoch-size 100000
}

function parse-cluster-config {
    cat > .coop-pab-cli/plutip-cluster-config
    make-exports
    mv .wallets/signing-key-$SUBMITTER_WALLET.skey .wallets/no-plutip-signing-key-$SUBMITTER_WALLET.skey
}

function make-exports {
    export GOD_WALLET=$(cat .coop-pab-cli/plutip-cluster-config | egrep "Wallet 1 PKH" | cut -d ":" -f 2 | xargs)
    export AA_WALLET=$(cat .coop-pab-cli/plutip-cluster-config | egrep "Wallet 2 PKH" | cut -d ":" -f 2 | xargs)
    export AUTH_WALLET=$(cat .coop-pab-cli/plutip-cluster-config | egrep "Wallet 3 PKH" | cut -d ":" -f 2 | xargs)
    export CERT_RDMR_WALLET=$(cat .coop-pab-cli/plutip-cluster-config | egrep "Wallet 4 PKH" | cut -d ":" -f 2 | xargs)
    export FEE_WALLET=$(cat .coop-pab-cli/plutip-cluster-config | egrep "Wallet 5 PKH" | cut -d ":" -f 2 | xargs)
    export SUBMITTER_WALLET=$(cat .coop-pab-cli/plutip-cluster-config | egrep "Wallet 6 PKH" | cut -d ":" -f 2 | xargs)
    export CARDANO_NODE_SOCKET_PATH=$(cat .coop-pab-cli/plutip-cluster-config | grep CardanoNodeConn | egrep -o '"[^"]+"' | sed s/\"//g)
}

function dump-env {
    export | egrep "WALLET|CARDANO_NODE_SOCKET_PATH"
}

function coop-genesis {
    make-exports
    cabal clean
    cabal run coop-pab-cli -- deploy --god-wallet $GOD_WALLET --aa-wallet $AA_WALLET
}

function coop-mint-cert-redeemers {
    make-exports
    cabal run coop-pab-cli -- mint-cert-redeemers --cert-rdmr-wallet $CERT_RDMR_WALLET --cert-rdmrs-to-mint 100
}

function coop-mint-authentication {
    make-exports
    NOW=$(get-onchain-time) && cabal run coop-pab-cli -- mint-auth --aa-wallet $AA_WALLET --certificate-valid-from $NOW --certificate-valid-to $(expr $NOW + 60 \* 60 \* 1000) --auth-wallet $AUTH_WALLET
}

function coop-run-tx-builder-grpc {
    make-exports
    cabal run coop-pab-cli -- tx-builder-grpc --auth-wallet $AUTH_WALLET --fee-wallet $FEE_WALLET
}

function coop-garbage-collect {
    make-exports
    cabal run coop-pab-cli -- garbage-collect --cert-rdmr-wallet $CERT-RDMR
}

function coop-get-state {
    make-exports
    cabal run coop-pab-cli -- get-state --any-wallet ${GOD_WALLET}
    cat .coop-pab-cli/coop-state.json | json_pp
}

function coop-poll-state {
    make-exports
    while true; do
        clear;
        coop-get-state;
        sleep 5;
    done;
}

function get-onchain-time {
    make-exports
    cabal run coop-pab-cli -- get-state --any-wallet ${GOD_WALLET} | grep "Current node client time range" | grep POSIXTime | egrep -o "[0-9]+"
}

function run-grpcui {
    make-exports
    grpcui -insecure -import-path ../coop-proto -proto ../coop-proto/tx-builder-service.proto localhost:5081
}

function coop-mint-fs {
    make-exports
    resp=$(grpcurl -insecure -import-path ../coop-proto -proto ../coop-proto/tx-builder-service.proto -d @ localhost:5081 coop.TxBuilder/createMintFsTx <<EOF
    {
        "factStatements": [
            {
                "fsId": "eW==",
                "gcAfter": {
                    "extended": "NEG_INF"
                },
                "fs": {
                    "pdint": "1337"
                }
            }
        ],
        "submitter": {
            "base16": "$SUBMITTER_WALLET"
        }
    }

EOF
           )
    rawTx=$(echo $resp | jq '.mintFsSuccess.mintFsTx | .cborHex = .cborBase16 | del(.cborBase16) | .description = "" | .type = "Tx BabbageEra"')
    echo $resp | jq '.mintFsSuccess.alreadyPublished'
    echo $resp | jq '.error'
    echo $rawTx > .coop-pab-cli/signed
}

function cardano-cli-sign {
    make-exports
    cardano-cli transaction sign --tx-file .coop-pab-cli/signed --signing-key-file .wallets/no-plutip-signing-key-$SUBMITTER_WALLET.skey --out-file .coop-pab-cli/ready
}

function cardano-cli-submit {
    make-exports
    cardano-cli transaction submit --tx-file .coop-pab-cli/ready  --mainnet
}

function coop-prelude {
    make-exports
    coop-genesis
    coop-mint-cert-redeemers
    coop-mint-authentication
    coop-run-tx-builder-grpc
}
