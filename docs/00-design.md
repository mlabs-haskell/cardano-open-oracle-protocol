---
tag:              "docs-00-design"
date:             2022-06-28
revision:         1
revision-notes:   "Updated the document's structure"
author:           FIXME
reviewers:        FIXME
status:           WIP
---

# Design Document

For the Cardano open oracle protocol, this document describes the design goals,
options considered, and the rationale for the design option selected for
implementation.

## Table of contents

- [Design Document](#design-document)
  - [Table of contents](#table-of-contents)
  - [Considered Designs](#considered-designs)
    - [Summary of pros and cons of the different proposals](#summary-of-pros-and-cons-of-the-different-proposals)
  - [Using a commitment scheme](#using-a-commitment-scheme)
    - [The proposed commitment scheme using a Merkle Tree (but not limited to it)](#the-proposed-commitment-scheme-using-a-merkle-tree-but-not-limited-to-it)
    - [Concurrency](#concurrency)
    - [Optimizations](#optimizations)
    - [Identification](#identification)
    - [Authenticity](#authenticity)
  - [Using a signature scheme](#using-a-signature-scheme)
  - [Open questions](#open-questions)
  - [Other relevant ideas](#other-relevant-ideas)
  - [Resources](#resources)

## Considered Designs

  1. [On-Chain Database](./proposals/00-onchain-db.md)

  2. [Off-Chain Database with On-Chain knowledge representation](./proposals/01-offchain-db-mtree.md)

  3. Off-Chain Database using signature scheme to validate correct information

  4. Off-Chain Database using signature scheme to validate correct information
     and On-Chain representation

### Summary of pros and cons of the different proposals

1. *Storing data on-chain*: imposes higher cost for the oracle maintainer, will
   probably be possible to use with the second idea, especially in cases where
   we have varying sizes in datums or datums that have to be used multiple times
   it would make sense to mix and match (benchmarking yet to be done)

2. *Storing data off-chain, using a commitment scheme*: this will be really
   cheap for the oracle maintainer as they only have to recreate a utxo with the
   e.g. the topmost hash of a merkle tree. However, the data will still have to
   be provided, so the user provides by paying for the data when providing them
   in the redeemer.

3. *Storing data off-chain, using a signature scheme*: this will be even cheaper
   reducing the cost of onchain storage to a utxo containing the public key of
   the oracle that is created exactly once

## Using a commitment scheme

### The proposed commitment scheme using a Merkle Tree (but not limited to it)

- the oracle owner serves an API that provides some data, e.g. price pairs
- a user requests a certain datum from the API (see e.g. URI scheme proposed in the
  data registry approach) and receives the datum, as well as a path into the merkle tree
  which is a datum that grows linear with the depth of the tree that corresponds to the
  utxo the oracle serves, the amount of datums contained in the tree will, however, be
  exponential to the depth
- a user provides the received datum within a redeemer together with the path
- a script will lookup the latest hash in the oracle that the redeemer claimed
  to be valid for the datum they provide and that the contract also deems trustworthy
- the script will now for $2^n$ possible datums do $2\cdot n$ hashes onchain
  to verify that the datum is indeed part of the commitment of the oracle

For a simple example of how we could map the onchain data to the offchain see `pure-impl -> OrcFax.MerkleTree`

### Concurrency

The issue: If an oracle updates frequently, it's possible that the update cycle *can* be shorter than the
cycle "retrieve data -> submit transaction -> transaction arrives onchain"

The solution: Have a rotating set of UTXOs to enable concurrency when
the oracle is updated frequently. There will be a set of UTXOs that where the oracle only updates the *oldest*
one, the other ones serve as a ring buffer. The user will have to provide which UTXO of a certain oracle
he referred to when submitting transaction and the script has to be aware of which oracle sets are considered
trustworthy from its standpoint. (e.g. it could look for an OrcFax-specific CurrencySymbol)

### Optimizations

- Merkle Trees need a significant amount of hashing, Las mentions that this can be speed up
  as soon as we have elliptic-curve-cryptography primitives onchain
  *note by Magnus: this has to be fleshed out by Las, maybe he can provide some resources*
- submitting multiple datums with a redeemer and a path each could get resource-consuming, especially if the
  datums are large or there are a lot of datums, proposed solution(s):
  - group together similar kinds of datums so they share a common path whose hash can then computed for all
    of them without duplicating work by (issue: this might not always be feasible as it will impose overheads
    in terms of size and script costs)
  - have commonly accessed datums sit very high in the tree, so the paths to them will be short onchain (if
    there's time-tagged data, you would probably also just push them as far up as possible)

### Identification

All the UTXOs would share a single identifying token to ensure authenticity
from the perspective of other protocols/scripts.
The datum will carry a timestamp. Ideas from the data-registry approach could be used here, as well.

### Authenticity

We can prove that the data has come from a specific website
using zero-knowledge proofs if the connection was served over TLS.
*note by Magnus: this has to be fleshed out by Las, maybe he can provide some resources*

## Using a signature scheme

This is the simplest and probably cheapest solution:

- the oracle signs some datum offchain and also distributes that data offchain, e.g. serves it at some API
- the user obtains a datum with the signature, submits it as the redeemer for some contract.
- the contract uses the new `verifySignature` and `serialiseData` primitive builtins to Plutus to verify the datum comes from a
  certain oracle
- the oracle only maintains a utxos with its own public key

## Open questions

- What could be use-cases of linking datums onchain?
- How do we make sure that if we compute a new datum from a number of other datums
  Trust is maintained (in the general case)?
- What are the exact costs for the different approaches; what would be a good way to mix and
  match the different ideas?
- How do the different ideas apply to decentralized oracles?

## Other relevant ideas

1. Additionally to the proposed ideas which are very tailored to centralized oracles which require trust, we also
   started looking into decentralized oracles (see [this paper in resources](https://eprint.iacr.org/2022/603.pdf)),
   especially the DOS-Network approach they explain very shortly seems interesting to Cardano, their proposed solution
   which entails a proof of work done by the oracles don't seem very feasible on Cardano (at least in Magnus' opinion)

## Resources

- Leonard Lys and Maria Potop-Butucaru, Distributed Blockchain Price Oracle, <https://eprint.iacr.org/2022/603.pdf>
