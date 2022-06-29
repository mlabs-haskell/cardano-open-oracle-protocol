---
tag: "docs-proposal-02-signature-scheme"
date:             2022-06-28
revision:
revision-notes:
author:           "cstml"
reviewers:        FIXME
status:           WIP
---

# Signature Scheme

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Signature Scheme](#signature-scheme)
  - [Using a signature scheme](#using-a-signature-scheme)

<!-- markdown-toc end -->


## Using a signature scheme

This is the simplest and probably cheapest solution:

- the oracle signs some datum offchain and also distributes that data offchain,
  e.g. serves it at some API
- the user obtains a datum with the signature, submits it as the redeemer for
  some contract.
- the contract uses the new `verifySignature` and `serialiseData` primitive
  builtins to Plutus to verify the datum comes from a certain oracle
- the oracle only maintains a utxos with its own public key
