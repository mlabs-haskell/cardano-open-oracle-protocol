---
tag: "docs-proposals-01-offchain-db-mtree"
date: 2022-06-28
revision: 
revision-summary:
author: "cstml"
reviewers:
status: WIP
---

# OffChain Database - Merkle Tree

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [OffChain Database - Merkle Tree](#offchain-database---merkle-tree)
  - [Using a commitment scheme](#using-a-commitment-scheme)
    - [The proposed commitment scheme using a Merkle Tree (but not limited to it)](#the-proposed-commitment-scheme-using-a-merkle-tree-but-not-limited-to-it)
    - [Concurrency](#concurrency)
    - [Optimizations](#optimizations)
    - [Identification](#identification)
    - [Authenticity](#authenticity)
  - [Open questions](#open-questions)

<!-- markdown-toc end -->

## Using a commitment scheme

### The proposed commitment scheme using a Merkle Tree (but not limited to it)

- the oracle owner serves an API that provides some data, e.g. price pairs
- a user requests a certain datum from the API (see e.g. URI scheme proposed in
  the data registry approach) and receives the datum, as well as a path into the
  merkle tree which is a datum that grows linear with the depth of the tree that
  corresponds to the utxo the oracle serves, the amount of datums contained in
  the tree will, however, be exponential to the depth
- a user provides the received datum within a redeemer together with the path
- a script will lookup the latest hash in the oracle that the redeemer claimed
  to be valid for the datum they provide and that the contract also deems
  trustworthy
- the script will now for $2^n$ possible datums do $2\cdot n$ hashes onchain to
  verify that the datum is indeed part of the commitment of the oracle

For a simple example of how we could map the onchain data to the offchain see
`pure-impl -> OrcFax.MerkleTree`

### Concurrency

The issue: If an oracle updates frequently, it's possible that the update cycle
*can* be shorter than the cycle "retrieve data -> submit transaction ->
transaction arrives onchain"

The solution: Have a rotating set of UTXOs to enable concurrency when the oracle
is updated frequently. There will be a set of UTXOs that where the oracle only
updates the *oldest* one, the other ones serve as a ring buffer. The user will
have to provide which UTXO of a certain oracle he referred to when submitting
transaction and the script has to be aware of which oracle sets are considered
trustworthy from its standpoint. (e.g. it could look for an OrcFax-specific
CurrencySymbol)

### Optimizations

- Merkle Trees need a significant amount of hashing, Las mentions that this can
  be speed up as soon as we have elliptic-curve-cryptography primitives onchain
  *note by Magnus: this has to be fleshed out by Las, maybe he can provide some
  resources*
- submitting multiple datums with a redeemer and a path each could get
  resource-consuming, especially if the datums are large or there are a lot of
  datums, proposed solution(s):
  - group together similar kinds of datums so they share a common path whose
    hash can then computed for all of them without duplicating work by (issue:
    this might not always be feasible as it will impose overheads in terms of
    size and script costs)
  - have commonly accessed datums sit very high in the tree, so the paths to
    them will be short onchain (if there's time-tagged data, you would probably
    also just push them as far up as possible)

### Identification

All the UTXOs would share a single identifying token to ensure authenticity from
the perspective of other protocols/scripts. The datum will carry a timestamp.
Ideas from the data-registry approach could be used here, as well.

### Authenticity

We can prove that the data has come from a specific website using zero-knowledge
proofs if the connection was served over TLS. *note by Magnus: this has to be
fleshed out by Las, maybe he can provide some resources*

## Open questions

- What could be use-cases of linking datums onchain?
- How do we make sure that if we compute a new datum from a number of other
  datums Trust is maintained (in the general case)?
- What are the exact costs for the different approaches; what would be a good
  way to mix and match the different ideas?
- How do the different ideas apply to decentralized oracles?
