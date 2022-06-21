# Design document

For the Cardano open oracle protocol, this document describes the design goals,
options considered, and the rationale for the design option selected for
implementation.

## Summary of the different concepts and ideas we came up with until now

The ideas we are proposing are

1. [**Storing data on-chain**](#an-onchain-data-registry): imposes higher cost for the oracle maintainer, will probably be 
   possible to use with the second idea, especially in cases where we have varying sizes in datums or datums that have 
   to be used multiple times it would make sense to mix and match (benchmarking yet to be done)
2. [**Storing data off-chain, using a commitment scheme**](#using-a-commitment-scheme): this will be really cheap for the 
   oracle maintainer as they only have to recreate a utxo with the e.g. the topmost hash of a merkle tree. However, 
   the data will still have to be provided, so the user provides by paying for the data when providing them in the redeemer.
3. [**Storing data off-chain, using a signature scheme**](#using-a-signature-scheme): this will be even cheaper reducing  
   the cost of onchain storage to a utxo containing the public key of the oracle that is created exactly once 

Also see: 
- [open questions](#open-questions)
- [other relevant ideas](#other-relevant-ideas)
- [resources](#resources)


## An onchain data registry

1. Data structured similarly to `Data.Row`, see [row-types](https://hackage.haskell.org/package/row-types)
2. Commitment scheme is possible 
3. [Product Nodes](#product-nodes)
4. Tabular form higher order nodes 

### Graph

```
    ┌───────────────┐  ┌────────────────┐ ┌───────────────┐
    │ ID: A         │  │ ID: B          │ │ ID: (C,D)     │
    │ Datum: A      │  │ Datum: B       │ │ Datum: [(C,D)]│
    │ Time: 1       │  │ Time: 1        │ │ Time: 1       │
    └───────┬───────┘  └─────┬──────────┘ └──┬────────────┘
            │                │               │
            │          ┌─────▼───────────┐   │
            └──────────► Transaction that◄───┘
                       │ requires info   │
                       │ A x B x C x D   │
                       │ at Time 1       │
                       └─────────▲───────┘
 ┌────────┐                      │
 │User    ├──────────────────────┘
 └────────┘
```

### Product Nodes 

- any user can combine information from any `n` nodes and manipulate it using a function as long as they can 
  prove that the combination maintains trust according to the specification of trust given by the authority. 
  The result is provided in a new Datum.
- `f : [Data] -> ([Data] -> Data) -> MintingPolicy -> (CurrencySymbol, Data)` (rough sketch, this doesn't 
  correspond with the actual implementation) 
- can also be used by the actual authority to collapse datums that are similar into one maintaining reference 
  to old nodes

#### Graph
```
┌────────┬┬────────┬┬────────┐
│ID: idA ││ID: idB ││ID: idC │
│Datum: A││Datum: B││Datum: C│
│Time: 0 ││Time: 0 ││Time: 0 │
└──────┬─┴┴───┬────┴┴─┬──────┘
       │      │       │
     ┌─▼──────▼───────▼────┐
     │Combining Transaction│
     │using MintingPolicy  │
     │provided by authority│
     └─┬───────────────────┘
       │
     ┌─▼──────────────────────┐
     │Datum: A x B x C        │
     │ID: <cursym for given   │
     │     MintingPolicy>/    │
     │    <computed category>/│
     │    <new uid>           │
     └────────────────────────┘
```
*Note: making sure that the function we use to combine datums computes a datum that can be trusted 
is non-trivial in the general case*

### Interactions/ Shape of an onchain datum

1. Expiration and TTL 
  - validity interval: the interval in which the publisher deems a datum to be valid in
  - time to live: must be at least upperbound of validity interval; corresponds to the time 
    from when on the publisher will be garbage collecting the datum; useful for 
    gaining back funds.

2. Commitment Scheme
  - this can be the scheme proposed [beneath](#using-a-commitment-scheme) or basically any other commitment scheme, the designs 
    are not mutually exclusiveus

3. Naming scheme in the data registry; the datum-`URI`
  - two main requirements for the URI: representative (searchable), unique
  - <currency_symbol_per_provider>/<categorization>/<uid>
    - **currency_symbol_per_provider**: MintingPolicy for authority and specific information
      stream (or MintingPolicy provided by authority that allows for combination of datums
      without losing trust which is bound to rules defined by authority)
    - example: 
      "`<OrcFax-specify cursym>/nasdaq/goog/price/<uid identifying the query done by the oracle>`"
  - this corresponds to the `URI`-field in `UTXOData`

n. TODO: fill in the missing fields of the sample datatype and everything that might be in it

#### Draft of a possible Onchain representation of the datum

```haskell
data Arity = Primitive 
           | Multiple

data UTXOData (arity :: Arity) (isSequential :: Bool) = MkUTXOData
  -- | specifies whether you have a primitive datum or a composition
  --   of them
  { isPrimitive :: SBool isSequential
  -- | function(s) from the URI of the Datum, see URI type
  --   to the NFT identifying the UTXOData
  , typeId :: MkTypeId arity

  , metadata :: MetaData

  -- | the actual data attached to the UTXOData
  , datum :: [(URIHash, Data)] 

  -- | optional link back to predecessor nodes
  --   TODO: Is this necessary? Maybe it could be derived from the URI
  , predecessors :: MkPredecessor arity isSequential
  } 

data MetaData = MkMetaData
  -- | datum is valid from, until
  { validityInterval :: POSIXInterval
  -- | to gain back a part of the spent money for the locked
  --   UTXOs, onChainUntil >= validityInterval.upperBound
  , timeToLive :: Extended POSIXTime
  -- NOTE: this can also be `UpperBound` to include Closure information
  }

-- | The URI of a specific Datum
data URI = MkURI
  -- | the ID of a stream, e.g. the orcfax specific currency symbol
  { streamID :: CurrencySymbol
  -- | semantical category of the stream, e.g. "cardano/coin/prices"
  , categorization :: ByteString
  -- | the UID specialising the instance of a category of `dataStreamName`
  , dataUid :: ByteString
  }

-- | The (onchain-computable) hash of a datum-URI 
newtype URIHash = MkURIHash ByteString
newtype PrevURIHash = MkPrevURIHash URIHash

type family MkTypeId (a :: Arity) where 
  MkTypeId 'Primitive = (URI -> Value)
  MkTypeId 'Multiple = [URI -> Value]

type family MkPredecessor (arity :: Arity) (isSequential :: Bool) where 
  MkPredecessor _ 'True = Void 
  -- this doesn't make sense, this should be left out if isn't 
  -- sequential (rewrite in terms of extendable records) 
  MkPredecessor 'Primitive 'False = PrevURIHash
  MkPredecessor 'Multiple 'False = [(PrevURIHash, URIHash)]

data SBool b where 
  STrue :: SBool 'True
  SFalse :: SBool 'False
```

*To avoid unnecessary onchain data the `UTXOData` can be specialized to not contain a 
predecessor field (depending on the use-case a datum might not depend on a previous datum)*

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
- the contract uses the new `verifySignature` primitive builtin to Plutus to verify the datum comes from a 
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

- Leonard Lys and Maria Potop-Butucaru, Distributed Blockchain Price Oracle, https://eprint.iacr.org/2022/603.pdf
