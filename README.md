# Cardano open oracle protocol

## Introduction

The Cardano open oracle protocol (COOP) is a protocol complemented by
an open-source SDK for publishing and consuming on-chain data using Cardano
[CIP-31](https://developers.cardano.org/docs/governance/cardano-improvement-proposals/cip-0031/)
reference inputs. Reference inputs allow a data provider to publish a data point
once and multiple consumers to use the data point in on-chain dApp scripts,
without interfering with each other.

The purpose of this project is to allow developers in the Cardano ecosystem to run their own oracles by using the components and protocols provided by COOP.

Development of the COOP is led by [MLabs](https://mlabs.city/) with feedback and
direction provided by the [Orcfax](https://www.orcfax.link/about/) oracle
project which will implement the COOP on its platform.

This project was graciously funded from the Cardano Treasury in [Catalyst Fund
8](https://cardano.ideascale.com/c/idea/402572).

## Documentation

The protocol is described in further detail in design document:
[coop-docs/00-design](coop-docs/00-design.md).

## Getting Started

### Installing Nix

The COOP repository relies heavily on the [Nix Package Manager](https://nixos.org/download.html) for both development and package distribution.

To install run the following command:

```console
sh <(curl -L https://nixos.org/nix/install) --daemon
```

and follow the directions.

Make sure to enable [Nix Flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes) and IFD by editing either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` on your machine
and add the following configuration entries:

```yaml
experimental-features = nix-command flakes
allow-import-from-derivation = true
```

Optionally, to improve build speed, it is possible to set up a binary caches maintained by IOHK and Plutonomicon by setting additional configuration entries:

```yaml
substituters = https://cache.nixos.org https://hydra.iohk.io https://iohk.cachix.org https://cache.iog.io https://public-plutonomicon.cachix.org
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=
```

### Building and developing with Nix

Once Nix is installed, you should be able to seamlessly use the repository to develop, build and run packages.

Download the Git repository:

```console
git clone https://github.com/mlabs-haskell/cardano-open-oracle-protocol.git
```

If you use [direnv](https://direnv.net/) the Nix development shells will be automatically loaded depending on which project part you're working on.
Otherwise, each `.envrc` file in COOP sub-directories contain a proper Nix target you can use with the `nix develop` command.

For example `nix develop #dev-pab` will build a Nix development shell that has everything needed for developing and compiling the `coop-pab` component.

### Tutorial

This tutorial demonstrates how to create and operate your own COOP Publisher, and how users can eventually use your service to publish new Fact Statements provided.

#### 1. Preparing the environment

A Nix environment is provided with all the tools necessary to run, operate and use COOP .

Prepare the directories and open a provided Nix environment:

```console
$ mkdir coop-tutorial
$ cd coop-tutorial
$ nix develop github:mlabs-haskell/cardano-open-oracle-protocol#dev-tutorial
[coop-env ~ coop-tutorial] $
```

The environment should now have the following tools available:

- [cardano-node](https://github.com/input-output-hk/cardano-node#using-cardano-node) for running a Cardano network,
- [cardano-cli](https://github.com/input-output-hk/cardano-node/tree/master/cardano-cli) for orchestrating a `cardano-node`, building, signing and submitting transactions,
- [chain-index](https://github.com/input-output-hk/plutus-apps/tree/main/plutus-chain-index) for storing and indexing datums used by the [COOP Plutus Protocol](#todo),
- [local-cluster](https://github.com/mlabs-haskell/plutip/tree/master/local-cluster) for running a local/private Cardano network,
- [coop-pab-cli](#todo) for initializing and operating the [COOP Plutus protocol](#todo) and operating the [COOP TxBuilder](#todo) service,
- [coop-plutus-cli](#todo) for providing serialized Plutus programs (ie. on-chain scripts) that implement the [COOP Plutus protocol](#todo),
- [coop-publisher-cli](#todo) for running a [COOP Publisher](#todo) service that implements the [COOP Frontend protocols](#todo),
- [json-fs-store-cli](#todo) for running a generic JSON-based implementation of the [COOP FactStatementStore](#todo) service

and some other convenience utilities including some Bash functions that conveniently wrap the invocation of above mentioned services and command line tools.

#### 2. Running a local Cardano network

Let's first start by preparing and running a local Cardano network using the `local-cluster` utility tool:

```console
[coop-env ~ coop-tutorial] $ export CLUSTER_DIR=.local-cluster WALLETS=.wallets
[coop-env ~ coop-tutorial] $ mkdir $CLUSTER_DIR $CLUSTER_DIR/scripts $CLUSTER_DIR/txs $WALLETS
[coop-env ~ coop-tutorial] $ local-cluster --dump-info-json $CLUSTER_DIR/local-cluster-info.json \
                --wallet-dir $WALLETS --num-wallets 10 --utxos 5 \
                --chain-index-port 9084 \
                --slot-len 1s --epoch-size 100000
...
Cluster is running. Ctrl-C to stop.
```

This creates the directories needed for the `local-cluster` to work and starts a Cardano network with 10 wallets (made available in the `$WALLETS` directory) that will be used in the Protocol. You can use the provided `run-cluster` Bash function to run these commands for you (inspect the content with `type run-cluster`).

Let's leave the `local-cluster` process running in the foreground of the current shell and open a new `[coop-env ~ coop-tutorial]` shell session to continue with the tutorial.

The `local-cluster` created some wallets, let's assign them to environment variables that will be referenced throughout this tutorial:

```console
[coop-env ~ coop-tutorial] $ make-exports
[coop-env ~ coop-tutorial] $ show-env | grep PKH
show-env | grep PKH
declare -x AA_PKH="319a165e8cb4c2c3eb898334ac3579eed75bcbb9a274f9ff259e74e3"
declare -x AUTH_PKH="46103a3b0671460efa35aee0f97c27d0a6b97bf59271663db7cd3d04"
declare -x CERT_RDMR_PKH="07c0bed25705dbaeb17ff53553035ddace3fa7a12ca75315ece8583b"
declare -x FEE_PKH="e6250649bed46e3b9343664f543e8ec3ba3eb01128be6b82a0491799"
declare -x GOD_PKH="c12aacc2604e89cd5dac1fb1e324ad552df1b18e2bd4230e8e15cfd5"
declare -x SUBMITTER_PKH="b7e59f40866e6ec88635343b9cc285043d344afbbe001ae645db0553"
```

Output shows some named wallets with their base16 public keys hash identifier. The `SUBMITTER_PKH` is the only wallet not used by the `COOP Publisher` that belongs to the user. In fact, we need to hide this wallet from the `local-cluster` as to emulate a real scenario:

```console
[coop-env ~ coop-tutorial] $ mv $WALLETS/signing-key-"$SUBMITTER_PKH".skey $WALLETS/my-signing-key-"$SUBMITTER_PKH".skey
```

All other are essential wallets are owned by the COOP Publisher and used throughout its lifecycle. We'll revisit their meaning as we progress through the tutorial.

The `make-exports` and `show-env` are provided Bash functions that wrap the parsing of `local-cluster` information and set the appropriate environment variables.

#### 3. Initializing the Protocol

We're ready now to perform the [COOP Plutus protocol - Genesis](#todo) using the `coop-pab-cli` command line tool:

```console
[coop-env ~ coop-tutorial] $ export COOP_PAB_DIR=.coop-pab-cli && mkdir $COOP_PAB_DIR
[coop-env ~ coop-tutorial] $ coop-pab-cli deploy --god-wallet $GOD_PKH --aa-wallet $AA_PKH
...
[CONTRACT] [INFO [Any]] deployCoop: Finished
```

At this point a `$COOP_PAB_DIR/coop-deployment.json` file was created that contains all the Plutus scripts associated with the `COOP Publisher`.

> NOTE:
> The `coop-deployment.json` file is intended to be shared with the users of the Protocol to enable them to assert proper script addresses and token authenticity.

The [GOD wallet](#todo) can be discarded after the Protocol Genesis and the [Authentication Authority aka AA wallet](#todo) takes the role as the root wallet of the Protocol that has the ability to issue new [Authentication Tokens](#todo) to [Authenticator wallets](#todo). More on that later...

> NOTE:
> The [Authentication Authority](#todo) wallets MUST be kept safe as their compromise impacts the integrity of the entire system. Trust in a particular COOP Publisher eventually reduces to this wallet.

Continuing, we should be able to already inspect he state of the Protocol by using a provided `coop-get-state` bash function:

```console
[coop-env ~ coop-tutorial] $ coop-get-state
getState: Success
{
  "cs'certificates": [],
  "cs'currentTime": [
    {
      "getPOSIXTime": 1668599399000
    },
    {
      "getPOSIXTime": 1668599400000
    }
  ],
  "cs'factStatements": []
}
```

As we can see there's currently nothing of interest there. The `cs'certificates` contains a list of [Certificates](#todo) available in the Protocol, and the `cs'factStatements` contains a list of all the published [Fact Statements](#todo). `cs'currentTime` is included for convenience to understand the on-chain time.

Now, it's time to issue [Authentication tokens](#todo) to [Authenticator](#todo) wallets:

```console
[coop-env ~ coop-tutorial] $ coop-pab-cli mint-cert-redeemers \
                                --cert-rdmr-wallet $CERT_RDMR_PKH \
                                --cert-rdmrs-to-mint 100
...
CONTRACT] [INFO [Any]] mintCertR: Finished
mintCertRdmrs: Minted $CERT-RDMR tokens with AssetClass

[coop-env ~ coop-tutorial] $ NOW=$(get-onchain-time) && coop-pab-cli mint-auth \
                                --aa-wallet $AA_PKH \
                                --certificate-valid-from $NOW \
                                --certificate-valid-to "$(expr $NOW + 60 \* 60 \* 1000)" \
                                --auth-wallet $AUTH_PKH
...
mintAuth: Minted $CERT
mintAuth: Minted $AUTH
```

The `coop-pab-cli mint-cert-redeemers` issues [Certificate redeemer tokens](#todo) to a special wallet that will be used in `coop-pab-cli garbage-collect` command to 'garbage collect' obsolete [Certificates](#todo) and is a prerequisite to `coop-pab-cli mint-auth` transaction. These tokens are never depleted.

The `coop-pab-cli mint-auth` is the most involved command in the protocol, it's intended to be used by the COOP Publisher operator on a regular basis to issue new 'ephemeral' [Authentication tokens](#todo) that are used to authenticate publishing of each new Fact Statement. Once depleted, they have to be replenished with this command and it's up to the Operator to manage when and how many are issued, a decision based on considering the security exposure of the [Authentication wallets](#todo) and the publishing request load.

The command takes in the [Authentication Authority wallet](#todo) that authorizes the issuance of a new authentication tokens to an [Authenticator wallet](#todo), setting the certificate validity to 1 HOUR from 'now', after which this authentication batch, meaning both [Certificate](#todo) and associated [Authentication tokens](#todo) become invalid and can be discarded.

> NOTE:
> Authentication tokens that are associated with an expired Certificate cannot be used in the Protocol any longer.

Since all the [Authentication tokens](#todo) are sent in batch to a single UTxO held by the [Authenticator wallets](#todo) we provide a utility to redistribute these tokens in separate UTxOs:

```console
[coop-env ~ coop-tutorial] $ coop-pab-cli redistribute-auth --auth-wallet $AUTH_PKH
...
redistributeAuth: Redistributed outputs for Authenticator
```

[Authentication tokens](#todo) are spend by [Fact Statement Minting transactions](#todo) to denote the 'authenticity' of the information provided in produced [Fact Statement UTxOs](#todo).
They are also associated with a [Certificate](#todo) that provides information on the time validity of [Authentication tokens](#todo) used in a [Fact Statement Minting transactions](#todo).

> NOTE:
> Authenticator wallets are so called 'hot-wallets' used when servicing each Fact Statement Publishing request, as such the Protocol designed a mitigation using [Certificates](#todo) that limit the impact a compromised [Authentication wallet](#todo) can have on the integrity of the Protocol.

Before we proceed, let's check in on the state of our Protocol now that we actually introduced our first action:

```console
[coop-env ~ coop-tutorial] $ coop-get-state

{
   "cs'certificates" : [
      {
         "cert'id" : "7279672bf427c10d43492f41ab3af02a8bcb97d9777539fc5eae0b108850c3ce",
         "cert'redeemerAc" : {
            "unAssetClass" : [
               {
                  "unCurrencySymbol" : "6b14c29615e356edfce1eeb652b703daa7c246bd52fa8d87c17aafaf"
               },
               "c639e2f8b64d6a0bdf1d48de48d832c57342e7980d6a4e98df92ef8c2c54ce75"
            ]
         },
         "cert'validity" : {
            "ivFrom" : [
               {
                  "contents" : {
                     "getPOSIXTime" : 1668599835000
                  },
                  "tag" : "Finite"
               },
               true
            ],
            "ivTo" : [
               {
                  "contents" : {
                     "getPOSIXTime" : 1668603435000
                  },
                  "tag" : "Finite"
               },
               true
            ]
         }
      }
   ],
   "cs'currentTime" : [
      {
         "getPOSIXTime" : 1668601830000
      },
      {
         "getPOSIXTime" : 1668601831000
      }
   ],
   "cs'factStatements" : []
}
```

As we can see a new [Certificate](#todo) has been successfully issued.

#### 4. Running a TxBuilder gRPC service

We're finally ready to run the first COOP service, namely the [TxBuilder gRPC](#todo) back-end service that has the responsibility of building the COOP Cardano transactions:

```console
[coop-env ~ coop-tutorial] $ generate-keys $COOP_PAB_DIR
[coop-env ~ coop-tutorial] $ coop-pab-cli tx-builder-grpc --auth-wallet $AUTH_PKH --fee-wallet $FEE_PKH
```

The provided `generate-keys` Bash function will initialize the TLS keys and certificates used by the gRPC service.
The service needs access to [Authenticator wallets](#todo) as it provides signatures for the transactions, and a [Fee wallet](#todo) to send the service fees to.

> NOTE:
> A [Fee wallet](#todo) is where the COOP Publisher receives the fees after a successful [Fact Statement Publishing](#todo).

You can inspect and interact with the service using the gRPC utilities provided in the environment ([grpcurl](https://github.com/fullstorydev/grpcurl) and [grpcui](https://github.com/fullstorydev/grpcui)).

Let's leave the `tx-builder-grpc` process running in the foreground of the current shell and open a new `[coop-env ~ coop-tutorial]` shell session to continue with the tutorial.

#### 5. Running a FactStatementStore gRPC service

COOP provides a low-scale implementation of the [FactStatementStore gRPC](#todo) back-end service, namely the [JSON Fact Statement Store](#todo) that, as the name suggests, enables COOP Publisher operators to conveniently maintain a store of JSON encoded Fact Statements that users can refer to and eventually publish.

First let's prepare and initialize the service:

```console
[coop-env ~ coop-tutorial] $ mkdir $JS_STORE_DIR
[coop-env ~ coop-tutorial] $ sqlite3 -batch $JS_STORE_DIR/json-store.db ""
[coop-env ~ coop-tutorial] $ json-fs-store-cli genesis --db $JS_STORE_DIR/json-store.db
[coop-env ~ coop-tutorial] $ generate-keys $JS_STORE_DIR
```

Let's also add some actual Fact Statements into the store, while we're here:

```console
[coop-env ~ coop-tutorial] $ json-fs-store-cli insert-fact-statement --db $JS_STORE_DIR/json-store.db \
                                --fact_statement_id "id1" \
                                --json '["apples", "oranges", "pears"]'
[coop-env ~ coop-tutorial] $ json-fs-store-cli insert-fact-statement --db $JS_STORE_DIR/json-store.db \
                                    --fact_statement_id "id2" \
                                    --json '{"name": "Drazen Popovic", "age": 35}'
[coop-env ~ coop-tutorial] $ json-fs-store-cli insert-fact-statement --db $JS_STORE_DIR/json-store.db \
                                    --fact_statement_id "id3" \
                                    --json '"Lorem ipsum"'
[coop-env ~ coop-tutorial] $ echo "SELECT * FROM fact_statements" | sqlite3 $JS_STORE_DIR/json-store.db
id1|["apples", "oranges", "pears"]
id2|{"name": "Drazen Popovic", "age": 35}
id3|"Lorem ipsum"
```

Now we simply start the service:

```console
[coop-env ~ coop-tutorial] $ json-fs-store-cli fact-statement-store-grpc --db $JS_STORE_DIR/json-store.db
```

You can inspect and interact with the service using the gRPC utilities provided in the environment ([grpcurl](https://github.com/fullstorydev/grpcurl) and [grpcui](https://github.com/fullstorydev/grpcui)).

Let's leave the `fact-statement-store-grpc` process running in the foreground of the current shell and open a new `[coop-env ~ coop-tutorial]` shell session to continue with the tutorial. We're almost there!

#### 6. Running a Publisher gRPC service

The [Publisher gRPC](#todo) is the principal fronted service that COOP users interact with as described in the [COOP Frontend protocols](#todo).
This service relies on the back-end services that we've already setup, namely the [TxBuilder gRPC](#todo) service and the [FactStatementStore gRPC](#todo) service.

It's straightforward to run:

```console
[coop-env ~ coop-tutorial] $ mkdir $COOP_PUBLISHER_DIR
[coop-env ~ coop-tutorial] $ generate-keys $COOP_PUBLISHER_DIR
[coop-env ~ coop-tutorial] $ coop-publisher-cli publisher-grpc
```

The default command line arguments are sufficient for our scenario.

You can inspect and interact with the service using the gRPC utilities provided in the environment ([grpcurl](https://github.com/fullstorydev/grpcurl) and [grpcui](https://github.com/fullstorydev/grpcui)).

Let's leave the `publisher-grpc` process running in the foreground of the current shell and open a new `[coop-env ~ coop-tutorial]` shell session to continue with the tutorial. That's the last shell I promise, now we get to finally publish some fact statements.

#### 7. Publishing a Fact Statement

With the COOP Publisher fully set-up, we're ready to have our users publish some Fact Statements.

The users find out the [Fact Statement Identifiers](#todo) in a way not prescribed by COOP.
The Oracle provides some kind of access to their [Fact Statement Store](#todo), for example by an additional API with some search features, or even allows users to request a new Fact Statement to be collected/computed and inserted into the store.
Regardless, the users must approach the [COOP Publisher](#todo) with a [Fact Statement Identifier](#todo) that the back-end can eventually retrieve from the [Fact Statement Store](#todo).

> NOTE:
> COOP Publisher works with Fact Statements available in some Oracle's Fact Statement Store.
> Each Fact Statement in a store should get their own unique identifier, but this responsibility falls under a concrete Fact Statement Store operator.

With that, we already know there are 3 Fact Statements in our [Fact Statement Store](#todo) we've set-up, namely `id1`, `id2` and `id3`.
Let's publish all three of these Fact Statements:

```console
[coop-env ~ coop-tutorial] $ REQ=$(cat <<EOF
    {
        "fsInfos": [
            {
                "fsId": "$(echo -ne id1 | base64)",
                "gcAfter": {
                    "extended": "NEG_INF"
                }
            },
            {
                "fsId": "$(echo -ne id2 | base64)",
                "gcAfter": {
                    "extended": "NEG_INF"
                }
            },
            {
                "fsId": "$(echo -ne id3 | base64)",
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
```

This prepares a request to be issued with [grpcurl](https://github.com/fullstorydev/grpcurl).
The request lists all the [Fact Statement Identifiers](#todo) we wish to publish, along with their desired validity time (after which they can be 'garbage collected').
In this particular case, we've set the time to `NEG_INF` meaning we can garbage collect it at any time after publishing.

> NOTE:
> Users can specify validity time for the Fact Statement UTxOs they created and adjust it to the needs of the dApps they are referenced with. Some Fact Statements are going to be short lived, and some long lived, that largely depends on how the Fact Statement is used by a Cardano dApp. Protocol enables Submitters to 'garbage collect' the obsolete Fact Statement UTxOs and reclaim the min UTxO Ada held within.

Let's issue a request against the [Publisher gRPC](#todo) service:

```console
[coop-env ~ coop-tutorial] $ RESP=$(echo $REQ | grpcurl -insecure -import-path $COOP_PROTO \
                            -proto $COOP_PROTO/publisher-service.proto -d @ \
                            localhost:5080 coop.publisher.Publisher/createMintFsTx
                            )
[coop-env ~ coop-tutorial] $ echo "$RESP" | jq '.info'
{
  "txBuilderInfo": {
    "publishedFsIds": [
      "aWQx",
      "aWQy",
      "aWQz"
    ]
  }
}
[coop-env ~ coop-tutorial] $ echo "$RESP" | jq '.error'
null
```

The [Publisher gRPC service](#todo) successfully serviced the request and returned a CBOR encoded Cardano transaction in the `mintFsTx` field of the response.
Let's format the transaction so [cardano-cli](https://github.com/input-output-hk/cardano-node/tree/master/cardano-cli) can understand it:

```console
[coop-env ~ coop-tutorial] $ echo "$RESP" | jq '.mintFsTx | .cborHex = .cborBase16 | del(.cborBase16) | .description = "" | .type = "Tx BabbageEra"' \
        > transaction-to-sign.json
```

> NOTE:
> Any Cardano wallet could be used as COOP provides a raw CBOR encoded transaction, we just used [cardano-cli](https://github.com/input-output-hk/cardano-node/tree/master/cardano-cli) for convenience.

Finally we can sign and submit the transaction:

```console
[coop-env ~ coop-tutorial] $ cardano-cli transaction sign \
                                --tx-file transaction-to-sign.json \
                                --signing-key-file $WALLETS/my-signing-key-"$SUBMITTER_PKH".skey \
                                --out-file transaction-to-submit.json
[coop-env ~ coop-tutorial] $ cardano-cli transaction submit \
                                --tx-file transaction-to-submit.json  --mainnet
Transaction successfully submitted.
```

The transaction was successfully submitted which means we should be able to see that reflected in the state of the Protocol:

```console
[coop-env ~ coop-tutorial] $ coop-get-state && jq ".[\"cs'factStatements\"]" $COOP_PAB_DIR/coop-state.json
[
  {
    "fd'fs": {
      "contents": [
        {
          "contents": "6170706c6573",
          "tag": "B"
        },
        {
          "contents": "6f72616e676573",
          "tag": "B"
        },
        {
          "contents": "7065617273",
          "tag": "B"
        }
      ],
      "tag": "List"
    },
    "fd'fsId": "696431",
    "fs'gcAfter": {
      "tag": "NegInf"
    },
    "fs'submitter": {
      "getPubKeyHash": "51bf399017a57f8873bf155f818feca7384c4618e5e6367450582325"
    }
  },
  {
    "fd'fs": {
      "contents": [
        [
          {
            "contents": "616765",
            "tag": "B"
          },
          {
            "contents": 35,
            "tag": "I"
          }
        ],
        [
          {
            "contents": "6e616d65",
            "tag": "B"
          },
          {
            "contents": "4472617a656e20506f706f766963",
            "tag": "B"
          }
        ]
      ],
      "tag": "Map"
    },
    "fd'fsId": "696432",
    "fs'gcAfter": {
      "tag": "NegInf"
    },
    "fs'submitter": {
      "getPubKeyHash": "51bf399017a57f8873bf155f818feca7384c4618e5e6367450582325"
    }
  },
  {
    "fd'fs": {
      "contents": "4c6f72656d20697073756d",
      "tag": "B"
    },
    "fd'fsId": "696433",
    "fs'gcAfter": {
      "tag": "NegInf"
    },
    "fs'submitter": {
      "getPubKeyHash": "51bf399017a57f8873bf155f818feca7384c4618e5e6367450582325"
    }
  }
]
```

Indeed, all the Fact Statements have been successfully published and can be used by any dApp by simply referencing the desired [Fact Statement UTxOs](#todo).
You can see the `fs'submitter` field set to the `SUBMITTER_PKH` which is important for when the Submitter decides to garbage collect the obsolete [Fact Statement UTxOs](#todo).
With that said, let's try and do exactly that...

#### 8. Garbage collecting obsolete Fact Statement UTxOs

The `fs'gcAfter` field of the [Fact Statement UTxO](#todo) datums denotes when that UTxO can be spent by the Submitter (denoted in `fs'submitter` field) that created that UTxO.

> NOTE:
> The `fs'gcAfter` validity time is a property of the Fact Statement UTxO, not the Fact Statement itself.
> It's merely used to enable Submitters manage reclaiming the [minimal UTxO Ada](https://cardano-ledger.readthedocs.io/en/latest/explanations/min-utxo-mary.html#minimum-ada-value-requirement) they had to pay for each [Fact Statement UTxO](#todo) they created.

Since, for the purpose of this tutorial, we've created the [Fact Statement UTxOs](#todo) that are considered 'immediately obsolete', we can proceed and garbage collect them, and thus reclaim the min UTxO Ada amount locked within.

```console
[coop-env ~ coop-tutorial] $ REQ=$(cat <<EOF
    {
        "fsIds": [
                 "$(echo -ne 'id1' | base64)",
                 "$(echo -ne 'id2' | base64)",
                 "$(echo -ne 'id3' | base64)"
                 ],
        "submitter": {
            "base16": "$SUBMITTER_PKH"
        }
    }
EOF
       )
[coop-env ~ coop-tutorial] $ RESP=$(echo $REQ \
                            | grpcurl -insecure -import-path $COOP_PROTO \
                                    -proto $COOP_PROTO/publisher-service.proto -d @ \
                                    localhost:5080 coop.publisher.Publisher/createGcFsTx)
[coop-env ~ coop-tutorial] $ echo "$RESP" | jq '.info'
{
  "txBuilderInfo": {
    "obsoleteFsIds": [
      "aWQx",
      "aWQy",
      "aWQz"
    ]
  }
}
```

The [Publisher gRPC service](#todo) successfully serviced the request and returned a CBOR encoded Cardano transaction in the `gcFsTx` field of the response.
Let's format the transaction so [cardano-cli](https://github.com/input-output-hk/cardano-node/tree/master/cardano-cli) can understand it, then sign and submit it:

```console
[coop-env ~ coop-tutorial] $ echo "$RESP" | jq '.gcFsTx | .cborHex = .cborBase16 | del(.cborBase16) | .description = "" | .type = "TxBodyBabbage"' > transaction-to-sign.json
[coop-env ~ coop-tutorial] $ cardano-cli transaction sign \
                            --tx-body-file transaction-to-sign.json \
                            --signing-key-file $WALLETS/my-signing-key-"$SUBMITTER_PKH".skey \
                            --out-file transaction-to-submit.json
[coop-env ~ coop-tutorial] $ cardano-cli transaction submit \
                            --tx-file transaction-to-submit.json  --mainnet
Transaction successfully submitted.
```

Great! Let's check the state of the Protocol now...

```console
[coop-env ~ coop-tutorial] $ coop-get-state && jq ".[\"cs'factStatements\"]" $COOP_PAB_DIR/coop-state.json
[]
```

As expected, there's no more Fact Statements available in the system.

#### 9. Garbage collecting obsolete Certificate UTxOs

The COOP Publisher operators can also manage reclaiming the [minimal UTxO Ada](https://cardano-ledger.readthedocs.io/en/latest/explanations/min-utxo-mary.html#minimum-ada-value-requirement) they had to pay for each [Certificate UTxO](#todo) they created when issuing new [Authentication tokens](#todo) with `coop-pab-cli mint-auth`.

Again, inspecting the state with `coop-get-state` we see there's an obsolete `Certificate UTxO` that can be garbage collected.

```console
[coop-env ~ coop-tutorial] $ coop-get-state

{
   "cs'certificates" : [
      {
         "cert'id" : "7279672bf427c10d43492f41ab3af02a8bcb97d9777539fc5eae0b108850c3ce",
         "cert'redeemerAc" : {
            "unAssetClass" : [
               {
                  "unCurrencySymbol" : "6b14c29615e356edfce1eeb652b703daa7c246bd52fa8d87c17aafaf"
               },
               "c639e2f8b64d6a0bdf1d48de48d832c57342e7980d6a4e98df92ef8c2c54ce75"
            ]
         },
         "cert'validity" : {
            "ivFrom" : [
               {
                  "contents" : {
                     "getPOSIXTime" : 1668599835000
                  },
                  "tag" : "Finite"
               },
               true
            ],
            "ivTo" : [
               {
                  "contents" : {
                     "getPOSIXTime" : 1668603435000
                  },
                  "tag" : "Finite"
               },
               true
            ]
         }
      }
   ],
   "cs'currentTime" : [
      {
         "getPOSIXTime" : 1668601830000
      },
      {
         "getPOSIXTime" : 1668601831000
      }
   ],
   "cs'factStatements" : []
}
```

Let's garbage collect it then...

```console
[coop-env ~ coop-tutorial] $ coop-pab-cli garbage-collect --cert-rdmr-wallet $CERT_RDMR_PKH
...
[CONTRACT] [INFO [Any]] burnCerts: Finished
garbageCollect: Collected $CERT UTxOs from @CertV using $CERT-RDMR tokens
```

This is where [Certificate redeemer wallets](#todo) come into play as they hold the tokens that the the verifying Plutus script checks when validating the consumption of its outputs.

```console
[coop-env ~ coop-tutorial] $ coop-get-state
{
   "cs'certificates" : [ ],
   "cs'currentTime" : [
      {
         "getPOSIXTime" : 1668608405000
      },
      {
         "getPOSIXTime" : 1668608406000
      }
   ],
   "cs'factStatements" : []
}
```

And we've made the full circle :)

## TODO

Catch all link for non-implemented.
