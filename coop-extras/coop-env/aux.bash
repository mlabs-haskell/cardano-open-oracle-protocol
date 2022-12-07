# shellcheck disable=SC2085,SC2155,SC2002,SC2003,SC2086
JS_STORE_DIR=.json-fs-store
COOP_PAB_DIR=.coop-pab-cli
COOP_PUBLISHER_DIR=.coop-publisher-cli
CLUSTER_DIR=.local-cluster # As specified in resources/pabConfig.yaml

WALLETS=.wallets

RESOURCES=resources # Symlinked by Nix env
COOP_PROTO=coop-proto # Symlinked by Nix env

function clean {
    rm -fR $JS_STORE_DIR
    rm -fR $COOP_PAB_DIR
    rm -fR $COOP_PUBLISHER_DIR
    rm -fR $CLUSTER_DIR
    rm -fR $WALLETS
}

# Generate TLS keys for Publisher, FactStatementStore and TxBuilder services
function generate-keys {
    openssl genrsa -out $1/key.pem 2048
    openssl req -new -key $1/key.pem -out $1/certificate.csr -subj "/C=US/ST=st/L=l/O=o/OU=IT/CN=localhost"
    openssl x509 -req -in $1/certificate.csr -signkey $1/key.pem -out $1/certificate.pem -extfile $RESOURCES/ssl-extensions-x509.conf -extensions v3_ca -subj "/C=US/ST=st/L=l/O=o/OU=IT/CN=localhost"
    openssl x509 -text -in $1/certificate.pem
}

# Prelude and run the FactStatementStore gRpc with a generic Json implementation
function run-js-fs-store {
    mkdir $JS_STORE_DIR
    sqlite3 -batch $JS_STORE_DIR/json-store.db ""
    json-fs-store-cli genesis --db $JS_STORE_DIR/json-store.db
    json-fs-store-cli insert-fact-statement --db $JS_STORE_DIR/json-store.db --fact_statement_id "someidA" --json "[1,2,3]"
    json-fs-store-cli insert-fact-statement --db $JS_STORE_DIR/json-store.db --fact_statement_id "someidB" --json "[4,5,6]"
    json-fs-store-cli insert-fact-statement --db $JS_STORE_DIR/json-store.db --fact_statement_id "someidC" --json "[7,8,9]"
	  echo "SELECT * FROM fact_statements" | sqlite3 $JS_STORE_DIR/json-store.db
    generate-keys $JS_STORE_DIR
    json-fs-store-cli fact-statement-store-grpc --db $JS_STORE_DIR/json-store.db
}

# Prelude and run the Plutip Local Cluster (cardano-node and wallet creation)
function run-cluster {
    mkdir $CLUSTER_DIR
    mkdir $CLUSTER_DIR/scripts
    mkdir $CLUSTER_DIR/txs
    mkdir $WALLETS
    local-cluster --dump-info-json $CLUSTER_DIR/local-cluster-info.json \
                  --wallet-dir $WALLETS \
                  -n 10 --utxos 5 \
                  --chain-index-port 9084 \
                  --slot-len 1s --epoch-size 100000
}

function on-load {
    if [ -f $CLUSTER_DIR/local-cluster-info.json ]; then
        make-exports
        if [ -f $WALLETS/signing-key-"$SUBMITTER_PKH".skey ]; then
            mv $WALLETS/signing-key-"$SUBMITTER_PKH".skey $WALLETS/my-signing-key-"$SUBMITTER_PKH".skey
        fi
    fi;
}

# Export the variables used across
function make-exports {
    export GOD_PKH=$(cat $CLUSTER_DIR/local-cluster-info.json | jq -r ".ciWallets[0][0]")
    export AA_PKH=$(cat $CLUSTER_DIR/local-cluster-info.json | jq -r ".ciWallets[1][0]")
    export AUTH_PKH=$(cat $CLUSTER_DIR/local-cluster-info.json | jq -r ".ciWallets[2][0]")
    export CERT_RDMR_PKH=$(cat $CLUSTER_DIR/local-cluster-info.json | jq -r ".ciWallets[3][0]")
    export FEE_PKH=$(cat $CLUSTER_DIR/local-cluster-info.json | jq -r ".ciWallets[4][0]")
    export SUBMITTER_PKH=$(cat $CLUSTER_DIR/local-cluster-info.json | jq -r ".ciWallets[5][0]")
    export CARDANO_NODE_SOCKET_PATH=$(cat $CLUSTER_DIR/local-cluster-info.json | jq -r ".ciNodeSocket")
}

# Prelude and run the TxBuilder gRpc
function run-tx-builder {
    make-exports
    coop-genesis
    coop-mint-cert-redeemers
    coop-mint-authentication
    coop-redist-auth
    generate-keys $COOP_PAB_DIR
    coop-run-tx-builder-grpc
}

function coop-genesis {
    mkdir $COOP_PAB_DIR
    coop-pab-cli deploy --god-wallet $GOD_PKH --aa-wallet $AA_PKH
}

function coop-mint-cert-redeemers {
    coop-pab-cli mint-cert-redeemers --cert-rdmr-wallet $CERT_RDMR_PKH --cert-rdmrs-to-mint 100
}

function coop-mint-authentication {
    NOW=$(get-onchain-time) && coop-pab-cli mint-auth --aa-wallet $AA_PKH --certificate-valid-from $NOW --certificate-valid-to "$(expr $NOW + 60 \* 60 \* 1000)" --auth-wallet $AUTH_PKH
}

function coop-redist-auth {
    coop-pab-cli redistribute-auth --auth-wallet $AUTH_PKH
}

function coop-run-tx-builder-grpc {
    coop-pab-cli tx-builder-grpc --auth-wallet $AUTH_PKH --fee-wallet $FEE_PKH
}

function show-env {
    export | grep -E "([A-Z_]+)_PKH|CARDANO_NODE_SOCKET_PATH"
}

function coop-garbage-collect {
    coop-pab-cli garbage-collect --cert-rdmr-wallet $CERT_RDMR_PKH
}

function coop-get-state {
    coop-pab-cli get-state --any-wallet $GOD_PKH
    cat $COOP_PAB_DIR/coop-state.json | jq
}

function coop-poll-state {
    while true; do
        clear;
        coop-get-state;
        sleep 5;
    done;
}

function fs-store-insert {
    json-fs-store-cli insert-fact-statement --db $JS_STORE_DIR/json-store.db --fact_statement_id "$1" --json "$2"
}

function get-onchain-time {
    coop-pab-cli get-state --any-wallet $GOD_PKH | grep "Current node client time range" | grep POSIXTime | grep -E -o "[0-9]+"
}

function run-grpcui {
    grpcui -insecure -import-path $COOP_PROTO -proto $COOP_PROTO/publisher-service.proto localhost:5080
}

function run-publisher {
    mkdir $COOP_PUBLISHER_DIR
    generate-keys $COOP_PUBLISHER_DIR
    make-exports
    coop-publisher-cli publisher-grpc
}

function run-all {
    run-cluster &
    while [ ! -f $CLUSTER_DIR/local-cluster-info.json ]; do sleep 1; done;
    make-exports
    mv $WALLETS/signing-key-"$SUBMITTER_PKH".skey $WALLETS/my-signing-key-"$SUBMITTER_PKH".skey
    run-js-fs-store &
    run-tx-builder &
    run-publisher &
}

function coop-mint-fs {
    make-exports
    req=$(cat <<EOF
    {
        "fsInfos": [
            {
                "fsId": "$(echo -ne someidA | base64)",
                "gcAfter": {
                    "extended": "NEG_INF"
                }
            },
            {
                "fsId": "$(echo -ne someidB | base64)",
                "gcAfter": {
                    "extended": "NEG_INF"
                }
            },
            {
                "fsId": "$(echo -ne someidC | base64)",
                "gcAfter": {
                    "extended": "NEG_INF"
                }
            }
        ],
        "submitter": {
            "base16": "$SUBMITTER_PKH"
        }
    }
EOF
          )
    resp=$(echo $req | grpcurl -insecure -import-path $COOP_PROTO -proto $COOP_PROTO/publisher-service.proto -d @ localhost:5080 coop.publisher.Publisher/createMintFsTx)
    rawTx=$(echo "$resp" | jq '.mintFsTx | .cborHex = .cborBase16 | del(.cborBase16) | .description = "" | .type = "Tx BabbageEra"')
    echo "$resp" | jq '.info'
    echo "$resp" | jq '.error'
    echo "$rawTx" > $COOP_PUBLISHER_DIR/signed
    if [ "$(echo $resp | jq "has(\"mintFsTx\")")" == true ]; then
        cardano-cli transaction sign --tx-file $COOP_PUBLISHER_DIR/signed --signing-key-file $WALLETS/my-signing-key-"$SUBMITTER_PKH".skey --out-file $COOP_PUBLISHER_DIR/ready
        cardano-cli transaction submit --tx-file $COOP_PUBLISHER_DIR/ready  --mainnet
    else
        echo "No transaction to submit"
    fi
}

function coop-gc-fs {
    make-exports
    req=$(cat <<EOF
    {
        "fsIds": [
                 "$(echo -ne 'id1' | base64)",
                 "$(echo -ne 'someidA' | base64)",
                 "$(echo -ne 'someidB' | base64)",
                 "$(echo -ne 'id2' | base64)",
                 "$(echo -ne 'id3' | base64)"
                 ],
        "submitter": {
            "base16": "$SUBMITTER_PKH"
        }
    }

EOF
       )
    resp=$(echo $req | grpcurl -insecure -import-path $COOP_PROTO -proto $COOP_PROTO/publisher-service.proto -d @ localhost:5080 coop.publisher.Publisher/createGcFsTx)
    rawTx=$(echo "$resp" | jq '.gcFsTx | .cborHex = .cborBase16 | del(.cborBase16) | .description = "" | .type = "TxBodyBabbage"')
    echo "$resp" | jq '.info'
    echo "$resp" | jq '.error'
    echo "$rawTx" > $COOP_PUBLISHER_DIR/signed
    if [ "$(echo $resp | jq "has(\"gcFsTx\")")" == true ]; then
        cardano-cli transaction sign --tx-body-file $COOP_PUBLISHER_DIR/signed --signing-key-file $WALLETS/my-signing-key-"$SUBMITTER_PKH".skey --out-file $COOP_PUBLISHER_DIR/ready
        cardano-cli transaction submit --tx-file $COOP_PUBLISHER_DIR/ready  --mainnet
    else
        echo "No transaction to submit"
    fi
}
