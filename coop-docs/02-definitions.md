# Definitions

## Roles

### User

- **Submitter** is a Publisher client that requests publishing of Fact Statements. Eventually submits a FSMintTx transaction on Cardano creating the FSUTxO
- **Consumer** is a Cardano DApp (Plutus programs + environment) that uses a published Fact Statement by referencing the FSUTxO

### Service

- **Publisher** is a service that services Submitter requests to publish Fact Statements. It builds and returns a COOP FSMintTx transaction to Submitters
- **Collector** is a service that interacts with a data collection backend and delivers fact statements to Publisher clients

## Fact Statement

- **Fact Statement ID** abbreviated FSID is an identifier assigned to Fact Statements that are unique within the Fact Statement Universe operated by the Oracle
- **Fact Statement Universe** is the totality of all Fact Statements managed by an Oracle
- **Fact Statement Search** is a feature provided by the Oracle that the User roles can use to search for Fact Statements to publish
- **Fact Statement UTxO** is a unspent transaction output locked at **@FsV** that contains a Fact Statement Consumers can reference
- **Min FSUTxO $ADA** is the Min UTxO $ADA locked at @FsV that can be collected by the Submitter

## Cardano

- [Min UTxO $ADA](https://docs.cardano.org/native-tokens/minimum-ada-value-requirement) each UTxO in Cardano must contain some minimal amount of ADA (see )
