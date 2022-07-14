# Design Document

This document describes the design selected for the implementation of the
_Cardano Open Oracle Protocol_ (COOP).

## Design goals

The main design goals for this oracle protocol are:

1. Financial sustainability – minimize the cost and deposit needed to post,
   maintain, and use on-chain fact statements, sharing them equitably across
   stakeholders.
2. Data accessibility – minimize the probability of fact statements referenced
   by users not being available for their dApp transactions.

There is a trade-off between these two goals. Optimizing for data accessibility
tends to require more storage on-chain—including older fact statements that may
be referenced by users that are lagging in their synchronization of the
blockchain—whereas financial sustainability would seek to reduce/recycle these
fact statements as redundancies. We seek a healthy balance between them.

## Context: enabling features in Vasil HFC event

Cardano's [Vasil hardfork combinator (HFC)
event](https://iohk.io/en/blog/posts/2022/07/04/cardano-s-approaching-vasil-upgrade-what-to-expect/),
expected on mainnet in August 2022, will introduce:

- [Reference inputs
  (CIP-31)](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0031) –
  transactions can reference some of their inputs instead of consuming them,
  keeping those inputs intact for other transactions to consume or reference.
  Previously, transactions were required to consume their inputs, making those
  inputs unavailable to other transactions.
- [Inline datums
  (CIP-32)](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0032) –
  transaction outputs will be able to store their datums directly. Previously,
  transaction outputs could only store hashes of datums, with the actual datums
  stored in the transaction body's (datum hash 🠖 datum) table. This incurred a
  significant burden on users, who would have to provide the datums
  corresponding to the datum hashes for the inputs used in their transactions.
  This burden translated into an infrastructure requirement for an off-chain
  index from datum hashes to datums for all utxos on the blockchain, to allow
  users to efficiently retrieve the datums they need to provide in their
  transactions. This burden is eliminated for transaction outputs with inline
  datums.
- [Reference scripts
  (CIP-33)](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033) –
  inline scripts may be attached to transaction outputs, and subsequent
  transactions can reference those outputs to access their scripts and use them
  during validation. Previously, transactions had to include all of their
  scripts in their transaction bodies, bloating transaction sizes and blockchain
  space usage. With reference scripts, a dApp can optimize its space usage
  on-chain by efficiently reusing a set of general and stable scripts across
  multiple transactions.

Reference inputs are fundamentally important in the context of oracles, which
are primarily concerned with the dissemination of information. Prior to
reference inputs, oracle designs had to contend with exclusive consumption and
mitigated the challenges of concurrent use by relying on off-chain coordination,
redundant publication, and explicit re-publication of spent outputs. With
reference inputs, oracles can publish information on-chain once and have users
access that information concurrently and non-exclusively. The blockchain ledger
thus evolves from its resource management roots to include information
management.

Reference inputs may limit our design flexibility with respect to sharing
publication costs/deposits equitably across stakeholders—once information is
published on-chain, and as long as it remains available on-chain, no
restrictions or conditions may be placed on anyone's ability to reference it in
their transactions. This means that information from reference-input-based
oracles is free to use after its first use. However, a sustainable business
model for such oracles may still possible, as shown in the design below.
Furthermore, freedom of information on the blockchain may itself be valuable
according to the values of the decentralization movement.

Inline datums and reference scripts are tools that may be useful in pursuit of
financial sustainability for the oracle protocol by reducing transaction costs
and infrastructure requirements. They are not essential to the oracle design,
but should prove valuable in practice.

## Design description

The following interaction lies at the core of this design. It is between a user
seeking to submit a dApp transaction referencing a timely fact statement from an
oracle feed of interest, and the oracle that is the authoritative source for
that oracle feed:

1. [**User inquiry.**](#user-inquiry) The user
   checks the blockchain ledger for the existence of an unspent transaction
   output (utxo) that contains a timely (i.e. non-expired for the purposes of
   the dApp the user wants to interact with) fact statement from the oracle
   feed. The oracle may choose to provide an off-chain service for such
   existence queries, so that users don't have to check the blockchain
   themselves directly.
   - If such a utxo exists, the user may reference it in the dApp transaction
     without further interaction with the oracle.

   - If the utxo doesn't exist, then the user initiates an interaction with the
     oracle by requesting the fact statement from the oracle and indicating a
     utxo controlled by the user that may be used to pay for transaction fees,
     deposits, and the oracle's service fee.
2. [**Oracle offer.**](#oracle-offer) Upon
   receiving the user's request, the oracle constructs, signs, and returns to
   the user a **publish** transaction with:
   - **User payment input** – an input from the user's wallet, containing
     payment.
   - **Fact statement output** – an output locked under the
     [recycling](#recycling-li) script, containing the requested fact statement
     and a token minted in the transaction to prove the fact statement's
     provenance from the oracle.
   - **Oracle payment output** – an optional output sent to the oracle's wallet,
     containing a payment towards the oracle.
   - **User change output** – an output sent to the user, containing the
     remainder of the user's payment utxo input.
3. [**User acceptance.**](#user-acceptance)
   Upon receiving the oracle-signed publish transaction, the user inspects it.
   If satisfied with its contents, the user adds her signature to the
   transaction and submits it to the blockchain.
4. [**Downstream use in dApps.**](#downstream-use-in-dapps) The user submits a
   dApp transaction referencing the fact statement. As explained below, the user
   may chain this transaction immediately after the publish transaction (if the
   user submitted it herself), without waiting for it to be confirmed on the
   blockchain. When a user references fact statements in a dApp, the dApp is
   responsible for verifying the provenance of the fact statements from oracles
   and their relevance to the dApp transaction.
5. [**Recycling.**](#recycling) The [fact statement output](#oracle-offer-li)
   from the publish transaction is locked by a script that allows it, after an
   oracle-specified **time-to-live (TTL)** deadline, to be spent in a
   **recycle** transaction that returns the tokens it contains back to the user
   that paid for the publish transaction.

The above interaction supports financial sustainability by allowing oracles to
share the on-chain publication costs with users, and optionally to generate a
profit for a healthy business model. It also allows utxo deposits (Cardano
requires minimum 2 ADA per utxo) to be recovered when the fact statement utxo's
TTL expires—thus the funds locked in utxo deposits can be bounded and the
ongoing variable cost for publication is only the transaction fee for the
publish transaction.

Data accessibility can be supported by setting the fact statement's TTL
appropriately to its user demand, so that most users can have a reasonable
expectation that it will be available when they attempt to reference it. For
example, the user demand for a fact statement from the "Current ADA price" feed
should decay rapidly—e.g. an hour after publication, no user is expected to
reference it as the current price to a dApp except for a user that is extremely
out-of-sync with the blockchain. Thus, a TTL of one hour (perhaps even less)
would be appropriate for this fact statement. In general, an oracle feed's TTL
policy should be calibrated to its semantic domain and target audience.

Note that all fact statements published for an oracle feed remain available on
the blockchain ledger, even after fact statement utxos are spent. The blockchain
contains the full history of all transactions and all outputs (spent or unspent)
that have ever existed since the genesis block that started the blockchain. The
difference between a spent transaction output and an unspent transaction output
(utxo) is that every transaction can only spend or reference outputs that are
unspent at the time of the transaction. Thus, an on-chain oracle feed is
naturally partitioned into an active set of fact statements (contained in
unspent transaction outputs) and an inactive set of archived fact statements.
Only the active fact statements can be referenced by dApps.

In a later iteration of this design, we may explore a potential mechanism to
**re-publish** archived fact statements back into the active set, effectively
reversing the fact statement's recycle transaction. Naively implemented, this
feature can undermine the financial sustainability of the protocol by allowing
users to avoid paying the oracle or sharing the publication cost, so more
sophisticated on-chain logic will likely be required to mitigate/eliminate this
risk.

## Protocol architecture

Here is the component architecture of the Cardano Open Oracle Protocol:

[TODO: Insert architecture diagram]

### User inquiry

We provide the following components for the user inquiry procedure.

Frontend:

- [ ] Basic display of the oracle's feeds, indicating for each fact statement
      whether it is published on-chain. For published fact statements, display
      their utxo references.

Backend API:

- [ ] Metadata queries of the oracle's data catalogue about the available oracle
      feeds and their descriptions/schemas.
- [ ] Data queries for the fact statements in the oracle's feeds, whether or not
      they are published on-chain.
- [ ] Index for the published/unpublished status of every fact statement in the
      oracle's feeds.
- [ ] Wrapper for the fact statement off-chain query.

Off-chain queries:

- [ ] Fact statement query on whether a fact statement for a given spacetime
      interval exists among the current utxos for a given oracle feed. If the
      fact statement is published on-chain, return a utxo reference that can be
      used to include it as a reference input in a transaction.

### Oracle offer

We provide the following components for the oracle offer procedure.

Frontend:

- [ ] Interface to request an oracle-signed publish transaction for a
      user-selected selected fact statement.

Backend API:

- [ ] Construct a publish transaction and sign it with the oracle's key.

Off-chain queries:

- [ ] Construct a publish transaction for a given fact statement.
- [ ] Sign a publish transaction using the oracle's key.

On-chain scripts:

- [ ] Oracle provenance token minting policy.
- [ ] Recycling utxo locking script.

### User acceptance

We provide the following components for the user acceptance procedure.

Frontend:

- [ ] Interpret an oracle-signed publish transaction and display clearly to the
      user.
- [ ] Sign the publish transaction with the user's wallet key and submit to the
      blockchain, via wallet software.

### Downstream use in dApps

We provide the following components to facilitate the use of oracle fact
statements in downstream dApps.

On-chain:

- [ ] General re-usable code to verify a fact statement's provenance from a
      given oracle.
- [ ] Example implementation for oracle feed whitelisting and time-validity
      constraints in a downstream dApp.

Backend:

- [ ] Ergonomic API for direct use by dApps. Downstream dApps should be able to
      use the backend API for user inquiries directly, without having to go
      through the user frontend. However, specialized API endpoints may be
      necessary to make this ergonomic. The intent is for dApps to be able to
      seamless integrate information and actions for oracle feeds in their own
      frontend interfaces.

### Recycling

We provide the following components for the recycling procedure.

Off-chain queries:

- [ ] Find all published fact statement utxos that a given user can recycle.
- [ ] Construct a recycling transaction to spend one or more fact statements
      that are recyclable by a given user, sending their tokens to the user's
      wallet.

Frontend:

- [ ] Display the user's recyclable published fact statements.
- [ ] Select recyclable published facts, draft a transaction to recycle them,
      sign with user's wallet and submit.

Backend (optional alternative):

- [ ] Implementation consideration: if recycling transaction fees are low and
      predictable (they should be), and if we require users to deposit the
      recycling transaction fee in the fact statement output from the publish
      transaction, then automated bots (operated by anyone) could recycle fact
      statement utxos and redeem user deposits on their behalf (returning the
      deposits to users). This would reduce the manual interaction burden on
      users of the oracle.

## On-chain implementation details

Only the [user acceptance](#user-acceptance), [downstream use in
dApps](#downstream-use-in-dapps), and [recycling](#recycling) procedures submit
transactions that modify the blockchain ledger. The oracle offer procedure
drafts a publish transaction that includes a monetary policy (executed during
**user acceptance**) and locks an output with a script (executed during
[recycling](#recycling)). Additionaly, downstream dApps should include the
re-usable on-chain code from this library for fact statement verification.

### Oracle provenance token minting policy

[TODO: @bladyjoker]

### Recycling utxo locking script

[TODO: @bladyjoker]

### Oracle provenance verification re-usable on-chain code

[TODO: @bladyjoker]

## Other implementation details

[TODO: @bladyjoker]
