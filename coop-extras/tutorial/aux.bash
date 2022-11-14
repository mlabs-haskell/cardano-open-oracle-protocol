# shellcheck disable=SC2085,SC2155,SC2002,SC2003,SC2086
JS_STORE_DIR=.json-fs-store
COOP_PAB_DIR=.coop-pab-cli
COOP_PUBLISHER_DIR=.coop-publisher-cli
CLUSTER_DIR=.local-cluster # As specified in resources/pabConfig.yaml

WALLETS=.wallets

RESOURCES=resources
COOP_PROTO=coop-proto

function clean {
    rm -fR $JS_STORE_DIR
    rm -fR $COOP_PAB_DIR
    rm -fR $COOP_PUBLISHER_DIR
    rm -fR $CLUSTER_DIR
    rm -fR $WALLETS
}

function make-dirs {
    mkdir $JS_STORE_DIR
    mkdir $COOP_PAB_DIR
    mkdir $COOP_PUBLISHER_DIR
    mkdir $CLUSTER_DIR
    mkdir $CLUSTER_DIR/scripts
    mkdir $CLUSTER_DIR/txs
    mkdir $WALLETS
}

# Generate TLS keys for Publisher, FactStatementStore and TxBuilder services
function generate-keys {
    openssl genrsa -out $1/key.pem 2048
    openssl req -new -key $1/key.pem -out $1/certificate.csr -subj "/C=US/ST=st/L=l/O=o/OU=IT/CN=localhost"
    openssl x509 -req -in $1/certificate.csr -signkey $1/key.pem -out $1/certificate.pem -extfile $RESOURCES/ssl-extensions-x509.conf -extensions v3_ca -subj "/C=US/ST=st/L=l/O=o/OU=IT/CN=localhost"
    openssl x509 -text -in $1/certificate.pem
}

# Prelude and run the FactStatementStore gRpc with a generic Json implementation
function prelude-js-fs-store {
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
function prelude-cluster {
    mkdir $CLUSTER_DIR
    mkdir $CLUSTER_DIR/scripts
    mkdir $CLUSTER_DIR/txs
    mkdir $WALLETS
    local-cluster --wallet-dir $WALLETS -n 10 --utxos 5 --chain-index-port 9084 --slot-len 1s --epoch-size 100000
}

# Run manually to parse the config outputted by local-cluster
function parse-cluster-config {
    cat > $CLUSTER_DIR/plutip-cluster-config
    make-exports
    # So BPI doesn't have access to it
    echo $SUBMITTER_PKH
    if [ -f $WALLETS/my-signing-key-$SUBMITTER_PKH.skey ];
      then echo "My key already setup"
      else
        echo "Hiding my key from the PAB"
        mv $WALLETS/signing-key-$SUBMITTER_PKH.skey $WALLETS/my-signing-key-$SUBMITTER_PKH.skey
    fi;
}

if [ -f $CLUSTER_DIR/plutip-cluster-config ];
  then make-exports;
  else echo "Run prelude-cluster and parse with parse-cluster-config" ;
fi;

# Export the variables used across
function make-exports {
    export GOD_PKH=$(cat $CLUSTER_DIR/plutip-cluster-config | grep -E "Wallet 1 PKH" | cut -d ":" -f 2 | xargs)
    export AA_PKH=$(cat $CLUSTER_DIR/plutip-cluster-config | grep -E "Wallet 2 PKH" | cut -d ":" -f 2 | xargs)
    export AUTH_PKH=$(cat $CLUSTER_DIR/plutip-cluster-config | grep -E "Wallet 3 PKH" | cut -d ":" -f 2 | xargs)
    export CERT_RDMR_PKH=$(cat $CLUSTER_DIR/plutip-cluster-config | grep -E "Wallet 4 PKH" | cut -d ":" -f 2 | xargs)
    export FEE_PKH=$(cat $CLUSTER_DIR/plutip-cluster-config | grep -E "Wallet 5 PKH" | cut -d ":" -f 2 | xargs)
    export SUBMITTER_PKH=$(cat $CLUSTER_DIR/plutip-cluster-config | grep -E "Wallet 6 PKH" | cut -d ":" -f 2 | xargs)
    export CARDANO_NODE_SOCKET_PATH=$(cat $CLUSTER_DIR/plutip-cluster-config | grep CardanoNodeConn | grep -E -o '"[^"]+"' | sed s/\"//g)
}

# Prelude and run the TxBuilder gRpc
function prelude-tx-builder {
    mkdir $COOP_PAB_DIR
    generate-keys $COOP_PAB_DIR
    make-exports
    coop-genesis
    coop-mint-cert-redeemers
    coop-mint-authentication
    coop-redist-auth
    coop-run-tx-builder-grpc
}

function coop-genesis {
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
    export | grep -E "_PKH|CARDANO_NODE_SOCKET_PATH"
}

function coop-garbage-collect {
    coop-pab-cli garbage-collect --cert-rdmr-wallet $CERT_RDMR_PKH
}

function coop-get-state {
    coop-pab-cli get-state --any-wallet $GOD_PKH
    cat $COOP_PAB_DIR/coop-state.json | json_pp
}

function coop-poll-state {
    while true; do
        clear;
        coop-get-state;
        sleep 5;
    done;
}

function get-onchain-time {
    coop-pab-cli get-state --any-wallet $GOD_PKH | grep "Current node client time range" | grep POSIXTime | grep -E -o "[0-9]+"
}

function run-grpcui {
    grpcui -insecure -import-path $COOP_PROTO -proto $COOP_PROTO/publisher-service.proto localhost:5080
}

function prelude-publisher {
    mkdir $COOP_PUBLISHER_DIR
    generate-keys $COOP_PUBLISHER_DIR
    make-exports
    coop-publisher-cli publisher-grpc
}

function coop-mint-fs {
    resp=$(grpcurl -insecure -import-path $COOP_PROTO -proto $COOP_PROTO/publisher-service.proto -d @ localhost:5080 coop.publisher.Publisher/createMintFsTx <<EOF
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
    rawTx=$(echo "$resp" | jq '.mintFsTx | .cborHex = .cborBase16 | del(.cborBase16) | .description = "" | .type = "Tx BabbageEra"')
    echo "$resp" | jq '.info'
    echo "$resp" | jq '.error'
    echo "$rawTx" > $COOP_PUBLISHER_DIR/signed
    cardano-cli transaction sign --tx-file $COOP_PUBLISHER_DIR/signed --signing-key-file $WALLETS/my-signing-key-"$SUBMITTER_PKH".skey --out-file $COOP_PUBLISHER_DIR/ready
    cardano-cli transaction submit --tx-file $COOP_PUBLISHER_DIR/ready  --mainnet
}

function coop-gc-fs {
    resp=$(grpcurl -insecure -import-path $COOP_PROTO -proto $COOP_PROTO/publisher-service.proto -d @ localhost:5080 coop.publisher.Publisher/createGcFsTx <<EOF
    {
        "fsIds": [
          "$(echo -ne someidA | base64)",
          "$(echo -ne someidB | base64)",
          "$(echo -ne someidC | base64)"
          ],
        "submitter": {
            "base16": "$SUBMITTER_PKH"
        }
    }

EOF
        )
    rawTx=$(echo "$resp" | jq '.gcFsTx | .cborHex = .cborBase16 | del(.cborBase16) | .description = "" | .type = "TxBodyBabbage"')
    echo "$resp" | jq '.info'
    echo "$resp" | jq '.error'
    echo "$rawTx" > $COOP_PUBLISHER_DIR/signed
    cardano-cli transaction sign --tx-body-file $COOP_PUBLISHER_DIR/signed --signing-key-file $WALLETS/my-signing-key-"$SUBMITTER_PKH".skey --out-file $COOP_PUBLISHER_DIR/ready
    cardano-cli transaction submit --tx-file $COOP_PUBLISHER_DIR/ready  --mainnet
}
