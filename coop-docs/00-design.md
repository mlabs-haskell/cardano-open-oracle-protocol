# Design Document

This document describes the design selected for the implementation of the _Cardano Open Oracle Protocol_ (COOP).

## Design goals

The main design goals for this oracle protocol are:

1. Financial sustainability – minimize the cost and deposit needed to post, maintain, and use on-chain fact statements, sharing them equitably across stakeholders.
2. Data accessibility – minimize the probability of fact statements referenced by users not being available for their dApp transactions.
3. Security - minimize the exposure of cryptographic keys used in publishing fact statements.
4. Configurable trade-offs - enable managing trade-offs between the goals listed above.

## Features

- 1. Users CAN find published Fact Statements
  - Any user with access to the Cardano ledger can inspect unspent outputs at the fact statement validator script (ie. `@FsV`) associated with their trusted COOP Publisher to find fact statements
- 1.1 Users CAN publish a fact statement
  - Any user with access to a Cardano wallet and a COOP Publisher can publish a fact statement
- 1.2 Submitters can eventually claim 'min UTxO $ADA'
  - Each fact statement unspent output can be collected and spent by the Submitter after a pre-specified time had passed for the published fact statement
- 1.3 Submitter SHOULD NOT pay for failed Publishing
- 1.4 Consumer CAN verify that a FSUTxO and the Fact Statement it carries is authentic
- 1.5 Consumer CAN assert the availability of published fact statements
- 2. Publishers CAN collect fees
  - Any COOP Publisher can include a fee for their service by including it in the fact statement minting transaction ([caveat](#fee-escrow))
- 2.1 Publishers can collect 'min UTxO $ADA'
  - COOP Publisher can collect 'min UTxO $ADA' created by various operational transactions
- 2.2 Publishers can manage security
  - By using a novel authentication scheme COOP Publisher are able to 'authorize' fact statement minting using ephemeral authentication tokens (ie. $AA/$AUTH/$CERT)

### Collecting min UTxO ADAs

Unspent transaction outputs (ie. UTxOs) created by COOP transactions can be 'eventually' spent and collected by involved parties. This supports financial sustainability by allowing participants to recover minimum 2 ADA per UTxO that created during various COOP operations.

Note that all fact statements published remain available on the blockchain ledger, even after fact statement UTxOs are spent. However, once spent, a fact statement can't be referenced on-chain by dApps.

## TODO: Further details

- `plutus-architecture.md` - information about transactions, Plutus scripts and tokens involved in COOP.
- `service-architecture.md` - overview of all the services along with their interactions.

## Appendix

### Cardano features enabling oracles

Cardano's [Vasil hardfork combinator (HFC) event](https://iohk.io/en/blog/posts/2022/07/04/cardano-s-approaching-vasil-upgrade-what-to-expect/), expected on mainnet in August 2022, will introduce:

- [Reference inputs (CIP-31)](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0031) – transactions can reference some of their inputs instead of consuming them, keeping those inputs intact for other transactions to consume or reference. Previously, transactions were required to consume their inputs, making those inputs unavailable to other transactions.
- [Inline datums (CIP-32)](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0032) – transaction outputs will be able to store their datums directly. Previously, transaction outputs could only store hashes of datums, with the actual datums stored in the transaction body's (datum hash 🠖 datum) table. This incurred a significant burden on users, who would have to provide  the datums corresponding to the datum hashes for the inputs used in their transactions. This burden translated into an infrastructure requirement for an off-chain index from datum hashes to datums for all utxos on the blockchain, to allow users to efficiently retrieve the datums they need to provide in their transactions. This burden is eliminated for transaction outputs with inline datums.
- [Reference scripts (CIP-33)](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033) – inline scripts may be attached to transaction outputs, and subsequent transactions can reference those outputs to access their  scripts and use them during validation. Previously, transactions had to include all of their scripts in their transaction bodies, bloating transaction sizes and blockchain space usage. With reference scripts, a dApp can optimize its space usage on-chain by efficiently reusing a set of general and stable scripts across multiple transactions.

Reference inputs are fundamentally important in the context of oracles, which are primarily concerned with the dissemination of information. Prior to reference inputs, oracle designs had to contend with exclusive consumption and mitigated the challenges of concurrent use by relying on off-chain coordination, redundant publication, and explicit re-publication of spent outputs. With reference inputs, oracles can publish information on-chain once and have users access that information concurrently and non-exclusively. The blockchain ledger thus evolves from its resource management roots to include information management.

Reference inputs may limit our design flexibility with respect to sharing publication costs/deposits equitably across stakeholders—once information is published on-chain, and as long as it remains available on-chain, no restrictions or conditions may be placed on anyone's ability to reference it in their transactions. This means that information from reference-input-based oracles is free to use after its first use. However, a sustainable business model for such oracles may still possible, as shown in the design below. Furthermore, freedom of information on the blockchain may itself be valuable according to the values of the decentralization movement.

Inline datums and reference scripts are tools that may be useful in pursuit of financial sustainability for the oracle protocol by reducing transaction costs and infrastructure requirements. They are not essential to the oracle design, but should prove valuable in practice.

### Fee Escrow

**STATUS**: We're not implemeting the fee escrow due to added complexity, time lag and cost that it would introduce in the system.

[From Wikipedia](https://en.wikipedia.org/wiki/Escrow)

> An escrow is a contractual arrangement in which a third party (the stakeholder or escrow agent) receives and disburses money or property for the primary transacting parties, with the disbursement dependent on conditions agreed to by the transacting parties

Fee escrow protocol SHOULD lock the **Publishing Fee** in a @FeeV Plutus validator via a Fee transaction cosigned by both Publisher and Submitter. The Publisher can claim the Fee by proving a successful Publishing and Submitter can claim the Fee back by proving a failed Publishing.

#### Properties

1. Publisher MUST be able assert the existence of the valid Fee escrow by inspecting the submitted Fee transaction, before proceeding to doing any additional work.
2. Publisher MUST be able to claim the Fee by providing a proof of a successful Publishing.
3. Submitter MUST be able to claim the Fee by providing a proof of a failed Publishing.
4. Submitter MUST be able to reclaim the Cardano operational fees (ie. minUtxoAda).

Pros:

- Prop 2 is strongly satisfied

Cons:

- More transactions,
- Added complexity, time lag and cost.
