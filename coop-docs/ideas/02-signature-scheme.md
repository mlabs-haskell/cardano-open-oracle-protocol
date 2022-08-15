# Signature Scheme

## Using a signature scheme

This is the simplest and probably cheapest solution:

- the oracle signs some datum offchain and also distributes that data offchain,
  e.g. serves it at some API
- the user obtains a datum with the signature, submits it as the redeemer for
  some contract.
- the contract uses the new `verifySignature` and `serialiseData` primitive
  builtins to Plutus to verify the datum comes from a certain oracle
- the oracle only maintains a utxos with its own public key
