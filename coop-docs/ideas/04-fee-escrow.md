# Fee Escrow

**STATUS**: We're not implemeting the fee escrow due to added complexity, time lag and cost that it would introduce in the system.

[From Wikipedia](https://en.wikipedia.org/wiki/Escrow)

> An escrow is a contractual arrangement in which a third party (the stakeholder or escrow agent) receives and disburses money or property for the primary transacting parties, with the disbursement dependent on conditions agreed to by the transacting parties

Fee escrow protocol SHOULD lock the **Publishing Fee** in a @FeeV Plutus validator via a Fee transaction cosigned by both Publisher and Submitter. The Publisher can claim the Fee by proving a successful Publishing and Submitter can claim the Fee back by proving a failed Publishing.

Properties:

1. Publisher MUST be able assert the existence of the valid Fee escrow by inspecting the submitted Fee transaction, before proceeding to doing any additional work.
2. Publisher MUST be able to claim the Fee by providing a proof of a successful Publishing.
3. Submitter MUST be able to claim the Fee by providing a proof of a failed Publishing.
4. Submitter MUST be able to reclaim the Cardano operational fees (ie. minUtxoAda).

Pros:

- Prop 2 is strongly satisfied

Cons:

- More transactions,
- Added complexity, time lag and cost.
