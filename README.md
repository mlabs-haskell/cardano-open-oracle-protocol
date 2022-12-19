<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
# Table of Contents

- [Cardano open oracle protocol](#cardano-open-oracle-protocol)
  - [Introduction](#introduction)
  - [Documentation](#documentation)
  - [Getting Started](#getting-started)
    - [Installing Nix](#installing-nix)
    - [Building and developing](#building-and-developing)
    - [Tutorial](#tutorial)
      - [1. Preparing the environment](#1-preparing-the-environment)
      - [2. Running a local Cardano network](#2-running-a-local-cardano-network)
      - [3. Initializing the Protocol](#3-initializing-the-protocol)
      - [4. Running a TxBuilder gRPC service](#4-running-a-txbuilder-grpc-service)
      - [5. Running a FactStatementStore gRPC service](#5-running-a-factstatementstore-grpc-service)
      - [6. Running a Publisher gRPC service](#6-running-a-publisher-grpc-service)
      - [7. Publishing a Fact Statement](#7-publishing-a-fact-statement)
      - [8. Garbage collecting obsolete Fact Statement UTxOs](#8-garbage-collecting-obsolete-fact-statement-utxos)
      - [9. Garbage collecting obsolete Certificate UTxOs](#9-garbage-collecting-obsolete-certificate-utxos)
      - [10. Referencing published Fact Statement in Consumer dApps](#10-referencing-published-fact-statement-in-consumer-dapps)

<!-- markdown-toc end -->

# Cardano open oracle protocol

## Introduction

The Cardano open oracle protocol (COOP) is a protocol complemented by
an open-source SDK for publishing and consuming on-chain data using Cardano
[CIP-31](https://developers.cardano.org/docs/governance/cardano-improvement-proposals/cip-0031/)
reference inputs. Reference inputs allow a data provider to publish a data point
once and multiple consumers to use the data point in on-chain dApp scripts,
without interfering with each other.

The purpose of this project is to allow developers in the Cardano ecosystem to
host and run their own COOP Publisher and integrate it into their broader
Oracle offerings.

Development of the COOP is led by [MLabs](https://mlabs.city/) with feedback and
direction provided by the [Orcfax](https://www.orcfax.link/about/) oracle
project which will implement the COOP on its platform.

This project was graciously funded from the Cardano Treasury in [Catalyst Fund
8](https://cardano.ideascale.com/c/idea/402572).

## Documentation

The protocol is described in further detail in the following documents

- [Design document](coop-docs/00-design.md) contains information about the overall goals of this project,
- [Plutus protocol](coop-docs/02-plutus-protocol.md) contains information about the wallets, tokens, minting policies, validators and transactions used in COOP and their relationship,
- [Frontend protocol](coop-docs/03-frontend-protocol.md) contains information about how users must interact with the COOP Publisher in order to publish new Fact Statements and garbage collect obsolete Fact Statements,
- [Backend protocol](coop-docs/04-backend-protocol.md) contains information on the back-end operations needed to serve the Frontend protocol.

## Getting Started

### Installing Nix

The COOP repository relies heavily on the [Nix Package
Manager](https://nixos.org/download.html) for both development and package
distribution.

To install run the following command:

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

and follow the instructions.

```sh
$ nix --version
nix (Nix) 2.8.0
```

> NOTE: The repository should work with Nix version greater or equal to 2.8.0.

Make sure to enable [Nix Flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes)
and IFD by editing either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` on
your machine and add the following configuration entries:

```yaml
experimental-features = nix-command flakes
allow-import-from-derivation = true
```

Optionally, to improve build speed, it is possible to set up a binary caches
maintained by IOHK and Plutonomicon by setting additional configuration entries:

```yaml
substituters = https://cache.nixos.org https://iohk.cachix.org https://cache.iog.io https://public-plutonomicon.cachix.org
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=
```

### Building and developing

Once Nix is installed, you should be able to seamlessly use the repository to
develop, build and run packages.

Download the Git repository:

```sh
git clone https://github.com/mlabs-haskell/cardano-open-oracle-protocol.git
```

To facilitate seamlessly moving between directories and associated Nix
development shells we use [direnv](https://direnv.net) and
[nix-direnv](https://github.com/nix-community/nix-direnv):

To install both using `nixpkgs`:

```sh
nix profile install nixpkgs#direnv
nix profile install nixpkgs#nix-direnv
```

Your shell and editors should pick up on the `.envrc` files in different
directories and prepare the environment accordingly. Use `direnv allow` to
enable the direnv environment and `direnv reload` to reload it when necessary.
Otherwise, each `.envrc` file in COOP sub-directories contain a proper Nix
target you can use with the `nix develop` command. For example `nix develop #dev-pab` will build a Nix development shell that has everything needed for
developing and compiling the `coop-pab` component.

Additionally, throughout the repository one can use the [pre-commit](https://pre-commit.com/) tool:

```sh
$ pre-commit run --all
cabal-fmt................................................................Passed
fourmolu.................................................................Passed
hlint....................................................................Passed
markdownlint.............................................................Passed
nix-linter...............................................................Passed
nixpkgs-fmt..............................................................Passed
shellcheck...............................................................Passed
```

to run all the code quality tooling specified in the [pre-commit-check config file](pre-commit-check.nix).
These `pre-commit` checks need to pass for a `git commit` to be successful.

### Tutorial

This tutorial demonstrates how to create and operate your own COOP Publisher,
and how users can eventually use your service to publish new Fact Statements
provided.

While working through the Tutorial feel free to explore and inspect various Bash
functions and command line tools used. For example, using the `type` standard
Bash function one can discover the definition of some other Bash functions:

```sh
[coop-env ~ coop-tutorial] $ type coop-get-state
coop-get-state is a function
coop-get-state () 
{ 
    coop-pab-cli get-state --any-wallet $GOD_PKH;
    cat $COOP_PAB_DIR/coop-state.json | json_pp
}
```

Additionally, each cli tool provided by COOP support a `--help` flag that
provides detailed explanation on the purpose of commands and their options:

```sh
[coop-env ~ coop-tutorial] $ coop-plutus-cli --help
[coop-env ~ coop-tutorial] $ coop-pab-cli --help
[coop-env ~ coop-tutorial] $ coop-publisher-cli --help
[coop-env ~ coop-tutorial] $ json-fs-store-cli --help
```

Since we're going to be running some services, it's useful to know which ports
are used by which processes, for example:

```sh
[coop-env ~ coop-tutorial] $ netstat -ntuap | grep LISTEN | grep -E "local-cluster|cardano-node|json-*|coop-*"
```

#### 1. Preparing the environment

A Nix environment is provided with all the tools necessary to run, operate and use COOP.

Prepare the directories and open a provided Nix environment:

```sh
$ mkdir coop-tutorial
$ cd coop-tutorial
$ nix develop github:mlabs-haskell/cardano-open-oracle-protocol#coop-env
[coop-env ~ coop-tutorial] $
```

The environment should now have the following tools available:

- [cardano-node](https://github.com/input-output-hk/cardano-node#using-cardano-node) for running a Cardano network,
- [cardano-cli](https://github.com/input-output-hk/cardano-node/tree/master/cardano-cli) for orchestrating a `cardano-node`, building, signing and submitting transactions,
- [chain-index](https://github.com/input-output-hk/plutus-apps/tree/main/plutus-chain-index) for storing and indexing datums used by the [COOP Plutus protocol](coop-docs/02-plutus-protocol.md),
- [local-cluster](https://github.com/mlabs-haskell/plutip/tree/master/local-cluster) for running a local/private Cardano network,
- [coop-pab-cli](coop-pab) for initializing and operating the [COOP Plutus Protocol](coop-docs/02-plutus-protocol.md) and operating the [COOP TxBuilder gRPC](coop-proto/tx-builder-service.proto) service,
- [coop-plutus-cli](coop-plutus) for providing serialized Plutus programs (ie. on-chain scripts) that implement the [COOP Plutus Protocol](coop-docs/02-plutus-protocol.md),
- [coop-publisher-cli](coop-publisher) for running a [COOP Publisher gRPC](coop-proto/publisher-service.proto) service that implements the [COOP Frontend protocol](coop-docs/03-frontend-protocol.md),
- [json-fs-store-cli](coop-extras/json-fact-statement-store) for running a generic JSON-based implementation of the [COOP FactStatementStore gRPC](coop-proto/fact-statement-store-service.proto) service

and some other convenience utilities including some Bash functions that conveniently wrap the invocation of above mentioned services and command line tools.

#### 2. Running a local Cardano network

Let's first start by preparing and running a local Cardano network using the `local-cluster` utility tool:

```sh
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

```sh
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

```sh
[coop-env ~ coop-tutorial] $ mv $WALLETS/signing-key-"$SUBMITTER_PKH".skey $WALLETS/my-signing-key-"$SUBMITTER_PKH".skey
```

All other essential wallets are owned by the COOP Publisher and are used throughout its lifecycle. We'll revisit their role as we progress through the tutorial.

The `make-exports` and `show-env` are provided Bash functions that wrap the parsing of `local-cluster` information and set the appropriate environment variables.

#### 3. Initializing the Protocol

We're ready now to perform the [COOP Plutus protocol genesis](coop-docs/02-plutus-protocol.md#protocol-genesis) using the `coop-pab-cli` command line tool. We prepare the working directory and run the cli:

```sh
[coop-env ~ coop-tutorial] $ export COOP_PAB_DIR=.coop-pab-cli && mkdir $COOP_PAB_DIR
[coop-env ~ coop-tutorial] $ coop-pab-cli deploy --god-wallet $GOD_PKH --aa-wallet $AA_PKH
...
[CONTRACT] [INFO [Any]] deployCoop: Finished
```

At this point a `$COOP_PAB_DIR/coop-deployment.json` file was created that contains all the Plutus scripts associated with the `COOP Publisher`.

> NOTE:
> The `coop-deployment.json` file is intended to be shared with the users of the Protocol to enable them to assert proper script addresses and token authenticity.

The [God wallet](coop-docs/02-plutus-protocol.md#god) can be discarded after the
[Protocol Genesis](coop-docs/02-plutus-protocol.md#protocol-genesis) and the
[Authentication Authority aka AA
wallet](coop-docs/02-plutus-protocol.md#authentication-authority) takes the role
as the root wallet of the Protocol that has the ability to issue new
[Authentication tokens](coop-docs/02-plutus-protocol.md#auth-token) to
[Authenticator wallets](coop-docs/02-plutus-protocol.md#authenticator). More on
that later...

> NOTE:
> The [Authentication Authority](coop-docs/02-plutus-protocol.md#authentication-authority) wallets MUST be kept safe as their compromise impacts the integrity of the entire system. Trust in a particular COOP Publisher eventually reduces to this wallet.

Continuing, we should be able to already inspect he state of the Protocol by using a provided `coop-get-state` bash function:

```sh
[coop-env ~ coop-tutorial] $ coop-get-state
getState: Success
```

```json
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

As we can see there's currently nothing of interest there.
The `cs'certificates` contains a list of
[Certificates](coop-docs/02-plutus-protocol.md#cert-validator) available in the
Protocol, and the `cs'factStatements` contains a list of all the published [Fact
Statements](coop-docs/02-plutus-protocol.md#fs-validator). `cs'currentTime` is
included for convenience to observe the on-chain time.

Now, it's time to issue [Authentication
tokens](coop-docs/02-plutus-protocol.md#auth-token) to [Authenticator
wallets](coop-docs/02-plutus-protocol.md#authenticator):

```sh
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

The `coop-pab-cli mint-cert-redeemers` issues [Certificate redeemer
tokens](coop-docs/02-plutus-protocol.md#cert-rdmr-token) to a special wallet
that will be used in `coop-pab-cli garbage-collect` command to 'garbage collect'
obsolete [Certificates](coop-docs/02-plutus-protocol.md#cert-validator) and is a
prerequisite to `coop-pab-cli mint-auth` transaction. These tokens are never
depleted.

The `coop-pab-cli mint-auth` is the most involved command in the protocol, it's
intended to be used by the COOP Publisher operator on a regular basis to issue
new 'ephemeral' [Authentication
tokens](coop-docs/02-plutus-protocol.md#auth-token) that are used to
authenticate publishing of each new Fact Statement. Once depleted, they have to
be replenished with this command and it's up to the Operator to manage when and
how many are issued, a decision based on considering the security exposure of
the [Authenticator wallets](coop-docs/02-plutus-protocol.md#authenticator) and
the publishing request load.

The command takes in the [Authentication Authority
wallet](coop-docs/02-plutus-protocol.md#authentication-authority) that
authorizes the issuance of a new authentication tokens to an [Authenticator
wallet](coop-docs/02-plutus-protocol.md#authenticator), setting the certificate
validity to 1 HOUR from 'now', after which this authentication batch, meaning
both [Certificates](coop-docs/02-plutus-protocol.md#cert-validator) and
associated [Authentication tokens](coop-docs/02-plutus-protocol.md#auth-token)
become invalid and can be discarded.

> NOTE:
> Authentication tokens that are associated with an expired Certificate cannot be used in the Protocol.

Since all the [Authentication
tokens](coop-docs/02-plutus-protocol.md#auth-token) are sent in batch to a
single UTxO held by the [Authenticator
wallets](coop-docs/02-plutus-protocol.md#authenticator) we provide a convenience
utility to redistribute these tokens in separate UTxOs:

```sh
[coop-env ~ coop-tutorial] $ coop-pab-cli redistribute-auth --auth-wallet $AUTH_PKH
...
redistributeAuth: Redistributed outputs for Authenticator
```

[Authentication tokens](coop-docs/02-plutus-protocol.md#auth-token) are spend by
[Fact Statement Publishing
transactions](coop-docs/02-plutus-protocol.md#mint-fact-statement-tx) to denote
the 'authenticity' of the information provided in produced [Fact Statement
UTxOs](coop-docs/02-plutus-protocol.md#fs-validator). They are also associated
with a [Certificate](coop-docs/02-plutus-protocol.md#cert-validator) that
provides information on the time validity of [Authentication
tokens](coop-docs/02-plutus-protocol.md#auth-token) used in a [Fact Statement
Publishing
transactions](coop-docs/02-plutus-protocol.md#mint-fact-statement-tx).

> NOTE:
> Authenticator wallets are so called 'hot-wallets' used when servicing Fact Statement Publishing requests, as such the Protocol designed a mitigation using [Certificates](coop-docs/02-plutus-protocol.md#cert-validator) that limit the impact a compromised [Authenticator wallet](coop-docs/02-plutus-protocol.md#authenticator) can have on the integrity of the Protocol.

Before we proceed, let's check in on the state of our Protocol now that we actually introduced our first action:

```sh
[coop-env ~ coop-tutorial] $ coop-get-state
getState: Success
```

```json
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

As we can see a new [Certificate](coop-docs/02-plutus-protocol.md#cert-validator) has been successfully issued.

#### 4. Running a TxBuilder gRPC service

We're finally ready to run the first COOP service, namely the [TxBuilder
gRPC](coop-proto/tx-builder-service.proto) back-end service that has the
responsibility of building the COOP Cardano transactions:

```sh
[coop-env ~ coop-tutorial] $ generate-keys $COOP_PAB_DIR
[coop-env ~ coop-tutorial] $ coop-pab-cli tx-builder-grpc --auth-wallet $AUTH_PKH --fee-wallet $FEE_PKH
```

The provided `generate-keys` Bash function will initialize the TLS keys and
certificates used by the gRPC service. The service needs access to
[Authenticator wallets](coop-docs/02-plutus-protocol.md#authenticator) as it
provides signatures for the transactions, and a [Fee
wallet](coop-docs/02-plutus-protocol.md#fee-collector) to send the service fees
to.

> NOTE:
> A [Fee wallet](coop-docs/02-plutus-protocol.md#fee-collector) is where the COOP Publisher receives the fees after a successful [Fact Statement Publishing](coop-docs/03-frontend-protocol.md).

You can inspect and interact with the service using the gRPC utilities provided
in the environment ([grpcurl](https://github.com/fullstorydev/grpcurl) and
[grpcui](https://github.com/fullstorydev/grpcui)).

Let's leave the `tx-builder-grpc` process running in the foreground of the
current shell and open a new `[coop-env ~ coop-tutorial]` shell session to
continue with the tutorial.

#### 5. Running a FactStatementStore gRPC service

COOP provides a low-scale implementation of the [FactStatementStore
gRPC](coop-proto/fact-statement-store-service.proto) back-end service, namely
the [JSON Fact Statement Store](coop-extras/json-fact-statement-store) that, as
the name suggests, enables COOP Publisher operators to conveniently maintain a
store of JSON encoded Fact Statements that users can refer to and eventually
publish.

First let's prepare and initialize the service:

```sh
[coop-env ~ coop-tutorial] $ export JS_STORE_DIR=.json-fs-store && mkdir $JS_STORE_DIR
[coop-env ~ coop-tutorial] $ sqlite3 -batch $JS_STORE_DIR/json-store.db ""
[coop-env ~ coop-tutorial] $ json-fs-store-cli genesis --db $JS_STORE_DIR/json-store.db
[coop-env ~ coop-tutorial] $ generate-keys $JS_STORE_DIR
```

Let's also add some actual Fact Statements into the store, while we're here:

```sh
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

```sh
[coop-env ~ coop-tutorial] $ json-fs-store-cli fact-statement-store-grpc --db $JS_STORE_DIR/json-store.db
```

You can inspect and interact with the service using the gRPC utilities provided
in the environment ([grpcurl](https://github.com/fullstorydev/grpcurl) and
[grpcui](https://github.com/fullstorydev/grpcui)).

Let's leave the `fact-statement-store-grpc` process running in the foreground of
the current shell and open a new `[coop-env ~ coop-tutorial]` shell session to
continue with the tutorial. We're almost there!

#### 6. Running a Publisher gRPC service

The [Publisher gRPC](coop-proto/publisher-service.proto) is the principal
fronted service that COOP users interact with as described in the [COOP Frontend
protocol](coop-docs/03-frontend-protocol.md). This service relies on the
back-end services that we've already setup, namely the [TxBuilder
gRPC](coop-proto/tx-builder-service.proto) service and the [FactStatementStore
gRPC](coop-proto/fact-statement-store-service.proto) service.

It's straightforward to run:

```sh
[coop-env ~ coop-tutorial] $ export COOP_PUBLISHER_DIR=.coop-publisher-cli && mkdir $COOP_PUBLISHER_DIR
[coop-env ~ coop-tutorial] $ generate-keys $COOP_PUBLISHER_DIR
[coop-env ~ coop-tutorial] $ coop-publisher-cli publisher-grpc
```

The default command line arguments are sufficient for our scenario.

You can inspect and interact with the service using the gRPC utilities provided
in the environment ([grpcurl](https://github.com/fullstorydev/grpcurl) and
[grpcui](https://github.com/fullstorydev/grpcui)).

Let's leave the `publisher-grpc` process running in the foreground of the
current shell and open a new `[coop-env ~ coop-tutorial]` shell session to
continue with the tutorial. That's the last shell I promise, now we get to
finally publish some fact statements.

#### 7. Publishing a Fact Statement

With the COOP Publisher fully set-up, we're ready to have our users publish some
Fact Statements (See[Publishing a Fact
Statement](coop-docs/03-frontend-protocol.md#publishing-a-fact-statement)).

The users find out the Fact Statement Identifiers in a way not prescribed by COOP.
The Oracle provides some kind of access to their Fact Statement Store, for
example by an additional API with some search features, or even allows users to
request a new Fact Statement to be collected/computed and inserted into the
store. Regardless, the users must approach the [COOP
Publisher](coop-proto/publisher-service.proto) with a `Fact Statement
Identifier` that the back-end can eventually retrieve from the `Fact Statement
Store`.

> NOTE:
> COOP Publisher works with Fact Statements available in some Oracle's Fact Statement Store.
> Each Fact Statement in a store should get their own unique identifier, but this responsibility falls under a concrete Fact Statement Store operator.

With that, we already know there are 3 Fact Statements in our Fact Statement
Store we've set-up, namely `id1`, `id2` and `id3`. Let's publish all three of
these Fact Statements:

```sh
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
The request lists all the Fact Statement Identifiers we wish to publish, along
with their desired validity time (after which they can be 'garbage collected').
In this particular case, we've set the time to `NEG_INF` meaning we can garbage
collect it at any time after publishing.

> NOTE:
> Users can specify validity time for the [Fact Statement UTxOs](coop-docs/02-plutus-protocol.md#fs-validator) they created and adjust it to the needs of the dApps they are referenced with.
> Some Fact Statements are going to be short lived, and some long lived, that largely depends on how the Fact Statement is used by a Cardano dApp.
> Protocol enables Submitters to 'garbage collect' obsolete [Fact Statement UTxOs](coop-docs/02-plutus-protocol.md#fs-validator) and reclaim the [Min UTxO Ada](https://docs.cardano.org/native-tokens/minimum-ada-value-requirement) held within.

Let's issue a request against the [Publisher gRPC](coop-proto/publisher-service.proto) service:

```sh
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

The [Publisher gRPC](coop-proto/publisher-service.proto) service successfully
serviced the request and returned a CBOR encoded Cardano transaction in the
`mintFsTx` field of the response. Let's format the transaction so
[cardano-cli](https://github.com/input-output-hk/cardano-node/tree/master/cardano-cli)
can understand it:

```sh
[coop-env ~ coop-tutorial] $ echo "$RESP" | jq '.mintFsTx | .cborHex = .cborBase16 | del(.cborBase16) | .description = "" | .type = "Tx BabbageEra"' \
        > transaction-to-sign.json
```

> NOTE:
> Any Cardano wallet could be used as COOP provides a raw CBOR encoded transaction, we just used [cardano-cli](https://github.com/input-output-hk/cardano-node/tree/master/cardano-cli) for convenience to demonstrate the concept.

Finally we can sign and submit the transaction:

```sh
[coop-env ~ coop-tutorial] $ cardano-cli transaction sign \
                                --tx-file transaction-to-sign.json \
                                --signing-key-file $WALLETS/my-signing-key-"$SUBMITTER_PKH".skey \
                                --out-file transaction-to-submit.json
[coop-env ~ coop-tutorial] $ cardano-cli transaction submit \
                                --tx-file transaction-to-submit.json  --mainnet
Transaction successfully submitted.
```

The transaction was successfully submitted which means we should be able to see that reflected in the state of the Protocol:

```sh
[coop-env ~ coop-tutorial] $ coop-get-state && jq ".[\"cs'factStatements\"]" $COOP_PAB_DIR/coop-state.json
```

```json
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

Indeed, all the Fact Statements have been successfully published and can be used
by any dApp by simply referencing the desired [Fact Statement
UTxOs](coop-docs/02-plutus-protocol.md#fs-validator). You can see the
`fs'submitter` field set to the `SUBMITTER_PKH` which is important for when the
Submitter decides to garbage collect the obsolete [Fact Statement
UTxOs](coop-docs/02-plutus-protocol.md#fs-validator). With that said, let's try
and do exactly that...

#### 8. Garbage collecting obsolete Fact Statement UTxOs

The `fs'gcAfter` field of the [Fact Statement
UTxO](coop-docs/02-plutus-protocol.md#fs-validator) datums denotes when that
UTxO can be spent by the Submitter (denoted in `fs'submitter` field) that
created that UTxO.

> NOTE:
> The `fs'gcAfter` validity time is a property of the Fact Statement UTxO, not the Fact Statement itself.
> It's merely used to enable Submitters manage reclaiming the [Min UTxO Ada](https://docs.cardano.org/native-tokens/minimum-ada-value-requirement) they had to pay for each [Fact Statement UTxO](coop-docs/02-plutus-protocol.md#fs-validator) they created.

Since, for the purpose of this tutorial, we've created the [Fact Statement
UTxOs](coop-docs/02-plutus-protocol.md#fs-validator) that are considered
'immediately obsolete', we can proceed and garbage collect them, and thus
reclaim the [Min UTxO
Ada](https://docs.cardano.org/native-tokens/minimum-ada-value-requirement)
amount locked within.

```sh
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

The [Publisher gRPC](coop-proto/publisher-service.proto) service successfully
serviced the request and returned a CBOR encoded Cardano transaction in the
`gcFsTx` field of the response. Let's format the transaction so
[cardano-cli](https://github.com/input-output-hk/cardano-node/tree/master/cardano-cli)
can understand it, then sign and submit it:

```sh
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

```sh
[coop-env ~ coop-tutorial] $ coop-get-state && jq ".[\"cs'factStatements\"]" $COOP_PAB_DIR/coop-state.json
[]
```

As expected, there's no more Fact Statements available in the system.

#### 9. Garbage collecting obsolete Certificate UTxOs

The COOP Publisher operators can also manage reclaiming the [Min UTxO
Ada](https://docs.cardano.org/native-tokens/minimum-ada-value-requirement) they
had to pay for each [Certificate
UTxO](coop-docs/02-plutus-protocol.md#cert-validator) they created when issuing
new [Authentication tokens](coop-docs/02-plutus-protocol.md#auth-token) with
`coop-pab-cli mint-auth`.

Again, inspecting the state with `coop-get-state` we see there's an obsolete
[Certificate UTxO](coop-docs/02-plutus-protocol.md#cert-validator) that can be
garbage collected.

```sh
[coop-env ~ coop-tutorial] $ coop-get-state
getState: Success
```

```json
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

```sh
[coop-env ~ coop-tutorial] $ coop-pab-cli garbage-collect --cert-rdmr-wallet $CERT_RDMR_PKH
...
[CONTRACT] [INFO [Any]] burnCerts: Finished
garbageCollect: Collected $CERT UTxOs from @CertV using $CERT-RDMR tokens
```

This is where [Certificate redeemer wallets](coop-docs/02-plutus-protocol.md#certificate-redeemer) come into play as they hold the tokens that the the verifying Plutus script checks when validating the consumption of its outputs.

```sh
[coop-env ~ coop-tutorial] $ coop-get-state
getState: Success
```

```json
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

#### 10. Referencing published Fact Statement in Consumer dApps

The COOP Publisher must announce the deployment file created after [COOP Plutus
protocol genesis](coop-docs/02-plutus-protocol.md#protocol-genesis). This file
contains the [Fact Statement minting
policy](coop-docs/02-plutus-protocol.md#fs-policy) script which is the `Currency
Symbol` the consuming dApps use to assert the authenticity and provenance of the
referenced [Fact Statement UTxOs](coop-docs/02-plutus-protocol.md#fs-validator).
