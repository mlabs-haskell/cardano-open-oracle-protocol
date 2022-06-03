# Design document

For the Cardano open oracle protocol, this document describes the design goals,
options considered, and the rationale for the design option selected for
implementation.

## Las's thoughts

### Concurrency

I discussed this together with George, and we came to this conclusion:

We should have a rotating set of UTXOs to enable concurrency when
the oracle is updated frequently.
Any time you update the oracle, you consume the oldest UTXO and create a new one
with the same tokens but a new datum.

### Identification

All the UTXOs would share a single identifying token to ensure authenticity
from the perspective of other protocols/scripts.
The datum will carry a timestamp.

### Past data

We can use commitment schemes to prove past data if necessary.
A very trivial example is a Merkle tree.

### Authenticity

We can prove that the data has come from a specific website
using zero-knowledge proofs if the connection was served over TLS.
