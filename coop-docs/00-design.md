# COOP Design Document

This document describes the design selected for the implementation of the _Cardano Open Oracle Protocol_ (COOP).

## Design goals

The main design goals for this oracle protocol are:

1. Financial sustainability – minimize the cost and deposit needed to post, maintain, and use on-chain fact statements, sharing them equitably across stakeholders.
2. Data accessibility – minimize the probability of fact statements referenced by users not being available for their dApp transactions.
3. Security - minimize the exposure of cryptographic keys used in publishing fact statements.
4. Configurable trade-offs - enable managing trade-offs between the goals listed above.

## Motivation

The following interaction lies at the core of this design.
It is between a user seeking to submit a dApp transaction referencing a timely Fact Statement from an Oracle feed of interest, and the Oracle that is the authoritative source for that oracle feed:

1. **User inquiry** - The user checks the Cardano ledger for the existence of an unspent transaction output (UTxO) that contains a timely (i.e. non-expired for the purposes of the dApp the user wants to interact with) Fact Statement from the Oracle feed. The Oracle may choose to provide an off-chain service for such existence queries, so that users don't have to check the blockchain themselves directly.
   - If such a UTxO exists, the user may reference it in the dApp transaction without further interaction with the Oracle.
   - If the UTxO doesn't exist, then the user initiates an interaction with the Oracle by requesting the Fact Statement from the Oracle and indicating a UTxO controlled by the user that may be used to pay for transaction fees, deposits, and the Oracle's service fee.
2. **Oracle offer** - Upon receiving the user's request, the Oracle constructs, signs, and returns to the user a **publish** transaction with:
   - **User payment input** – an input from the user's wallet, containing payment.
   - **Fact Statement output** – an output locked under the Fact Statement validator script, containing the requested Fact Statement and a token minted in the transaction to prove the Fact Statement's provenance from the Oracle.
   - **Oracle payment output** – an optional output sent to the Oracle's wallet, containing a payment towards the Oracle.
   - **User change output** – an output sent to the user, containing the remainder of the user's payment UTxO input.
3. **User acceptance** - Upon receiving the Oracle-signed publish transaction, the user inspects it. If satisfied with its contents, the user adds her signature to the transaction and submits it to the blockchain.
4. **Downstream use in dApps** - The user submits a dApp transaction referencing the Fact Statement. As explained below, the user may chain this transaction immediately after the publish transaction (if the user submitted it herself), without waiting for it to be confirmed on the blockchain. When a user references Fact Statements in a dApp, the dApp is responsible for verifying the provenance of the Fact Statements from Oracles and their relevance to the dApp transaction.
5. **Garbage collection** - The Fact Statement UTxOs from the publish transaction is locked by a script that allows it, after an Submitter-specified **time-to-live (TTL)** deadline, to  be spent in a **garbage collection** transaction that returns the tokens it contains back to the user that paid for the publish transaction.

The above interaction supports financial sustainability by allowing Oracles to share the on-chain publication costs with users, and optionally to generate a profit for a healthy business model. It also allows UTxO deposits (Cardano requires minimum 2 ADA per UTxO) to be recovered when the Fact Statement UTxO's TTL expires—thus the funds locked in UTxO deposits can be bounded and the ongoing variable cost for publication is only the transaction fee for the publish transaction.

Data accessibility can be supported by setting the Fact Statement's TTL appropriately to its user demand, so that most users can have a reasonable expectation that it will be available when they attempt to reference it. For example, the user demand for a Fact Statement from the "Current ADA price" feed should decay rapidly—e.g. an hour after publication, no user is expected to reference it as the current price to a dApp except for a user that is extremely out-of-sync with the blockchain. Thus, a TTL of one hour (perhaps even less) would be appropriate for this Fact Statement. In general, an Oracle feed's TTL policy should be calibrated to its semantic domain and target audience.

Note that all Fact Statements published for an Oracle feed remain available on the blockchain ledger, even after Fact Statement UTxOs are spent. The blockchain contains the full history of all transactions and all outputs (spent or unspent) that have ever existed since the genesis block that started the blockchain. The difference between a spent transaction output and an unspent transaction output (UTxO) is that every transaction can only spend or reference outputs that are unspent at the time of the transaction. Thus, an on-chain Oracle feed is naturally partitioned into an active set of Fact Statements (contained in unspent transaction outputs) and an inactive set of archived Fact Statements. Only the active Fact Statements can be referenced by dApps.

In a later iteration of this design, we may explore a potential mechanism to **re-publish** archived Fact Statements back into the active set, effectively reversing the Fact Statement's recycle transaction. Naively implemented, this feature can undermine the financial sustainability of the protocol by allowing users to avoid paying the Oracle or sharing the publication cost, so more sophisticated on-chain logic will likely be required to mitigate/eliminate this risk.

## Features

- 1 Users CAN publish Fact Statements
  - Any user with access to a Cardano wallet and a COOP Publisher can publish a Fact Statement.
- 1.1 Users CAN find published Fact Statements
  - Any user with access to the Cardano ledger can inspect Fact Statement UTxOs at the Fact Statement validator script associated with their trusted COOP Publisher to find published Fact Statements.
- 1.2 Users can eventually claim [Min UTxO Ada](https://docs.cardano.org/native-tokens/minimum-ada-value-requirement) held at Fact Statement UTxOs they created
  - Each Fact Statement unspent output can be collected and spent by the Submitter after a pre-specified time had passed for the published Fact Statement.
- 1.3 Users SHOULD NOT pay for failed publishing
  - The Fee is only collected if the Fact Statement Publishing transaction is successfully submitter by the user.
- 1.4 Users CAN assert the authenticity of published Fact Statements
  - Authentic Fact Statement UTxOs hold a Fact Statement token with a Currency Symbol that is announced by the COOP Publisher.
- 1.5 Users CAN assert the availability of published Fact Statements
  - Each Fact Statement UTxO is associated with a time after which it becomes obsolete and can be garbage collected. Before that time it's guaranteed these Fact Statements are going to be available to Consumer dApps.
- 2 Publishers CAN collect fees
  - Any COOP Publisher can include a fee for their service by including it in the Fact Statement minting transaction ([caveat](#fee-escrow)).
- 2.1 Publishers can collect [Min UTxO Ada](https://docs.cardano.org/native-tokens/minimum-ada-value-requirement) at UTxOs they created
  - COOP Publisher can collect [Min UTxO Ada](https://docs.cardano.org/native-tokens/minimum-ada-value-requirement) created by various operational transactions.
- 2.2 Publishers can dynamically manage security parameters
  - By using a novel authentication scheme COOP Publishers are able to 'authorize' Fact Statement minting using ephemeral authentication tokens
  

### Collecting min UTxO ADAs

Unspent transaction outputs (ie. UTxOs) created by COOP transactions can be 'eventually' spent and collected by involved parties.
This supports financial sustainability by allowing participants to recover [Min UTxO Ada](https://docs.cardano.org/native-tokens/minimum-ada-value-requirement) paid when creating UTxOs.

Note that all fact Statements published remain available on the blockchain ledger, even after Fact Statement UTxOs are spent.
However, once spent, a Fact Statement UTxOs can't be referenced on-chain by Consumer dApps.

### Cardano features enabling Oracles

Cardano's [Vasil hardfork combinator (HFC) event](https://iohk.io/en/blog/posts/2022/07/04/cardano-s-approaching-vasil-upgrade-what-to-expect/), expected on mainnet in August 2022, will introduce:

- [Reference inputs (CIP-31)](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0031) – transactions can reference some of their inputs instead of consuming them, keeping those inputs intact for other transactions to consume or reference. Previously, transactions were required to consume their inputs, making those inputs unavailable to other transactions.
- [Inline datums (CIP-32)](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0032) – transaction outputs will be able to store their datums directly. Previously, transaction outputs could only store hashes of datums, with the actual datums stored in the transaction body's (datum hash 🠖 datum) table. This incurred a significant burden on users, who would have to provide  the datums corresponding to the datum hashes for the inputs used in their transactions. This burden translated into an infrastructure requirement for an off-chain index from datum hashes to datums for all utxos on the blockchain, to allow users to efficiently retrieve the datums they need to provide in their transactions. This burden is eliminated for transaction outputs with inline datums.
- [Reference scripts (CIP-33)](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033) – inline scripts may be attached to transaction outputs, and subsequent transactions can reference those outputs to access their  scripts and use them during validation. Previously, transactions had to include all of their scripts in their transaction bodies, bloating transaction sizes and blockchain space usage. With reference scripts, a dApp can optimize its space usage on-chain by efficiently reusing a set of general and stable scripts across multiple transactions.

Reference inputs are fundamentally important in the context of Oracles, which are primarily concerned with the dissemination of information. Prior to reference inputs, Oracle designs had to contend with exclusive consumption and mitigated the challenges of concurrent use by relying on off-chain coordination, redundant publication, and explicit re-publication of spent outputs. With reference inputs, Oracles can publish information on-chain once and have users access that information concurrently and non-exclusively. The blockchain ledger thus evolves from its resource management roots to include information management.

Reference inputs may limit our design flexibility with respect to sharing publication costs/deposits equitably across stakeholders—once information is published on-chain, and as long as it remains available on-chain, no restrictions or conditions may be placed on anyone's ability to reference it in their transactions. This means that information from reference-input-based Oracles is free to use after its first use. However, a sustainable business model for such Oracles may still possible, as shown in the design below. Furthermore, freedom of information on the blockchain may itself be valuable according to the values of the decentralization movement.

Inline datums and reference scripts are tools that may be useful in pursuit of financial sustainability for the Oracle protocol by reducing transaction costs and infrastructure requirements. They are not essential to the Oracle design, but should prove valuable in practice.

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

- Prop 1.1 is strongly satisfied

Cons:

- More transactions,
- Added complexity, time lag and cost.
