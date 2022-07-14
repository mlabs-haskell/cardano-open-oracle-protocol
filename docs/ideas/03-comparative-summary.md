# Comparative Summary

## Summary of pros and cons of the different proposals

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

## Other relevant ideas

1. Additionally to the proposed ideas which are very tailored to centralized
   oracles which require trust, we also started looking into decentralized
   oracles (see [this paper in
   resources](https://eprint.iacr.org/2022/603.pdf)), especially the DOS-Network
   approach they explain very shortly seems interesting to Cardano, their
   proposed solution which entails a proof of work done by the oracles don't
   seem very feasible on Cardano (at least in Magnus' opinion)
