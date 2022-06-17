# Design document

For the Cardano open oracle protocol, this document describes the design goals,
options considered, and the rationale for the design option selected for
implementation.

## Initial proposal/ concepts

### Vlads ideas

1. Data structured similarly to `Data.Row`, see [row-types](https://hackage.haskell.org/package/row-types)
2. Commitment scheme similar to [Merkle Trees](https://en.wikipedia.org/wiki/Merkle_tree)
3. [Product Nodes](#product-nodes)
4. Tabular form higher order nodes 

#### Graph
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
  - this can be the scheme proposed by Las or basically any other commitment scheme, the designs 
    are not mutually exclusive

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

## Open questions

- what could be use-cases of linking datums onchain
- how do we make sure that if we compute a new datum from a number of other datums 
  trust is maintained (in the general case)
- what are the exact costs for the different approaches; what would be a good way to mix and 
  match the different ideas

## Las' ideas

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

Simple example of how we could map the onchain data to the offchain

```haskell
-- | the tree that contains the data in form of a Bytesstring, that 
--   is basically the offchain datastructure
data Tree = Leaf ByteString | Node Tree Tree

-- | the hash representing a hash of a Tree at a certain depth
newtype MerkleTree = MerkleTree Hash


-- | hashes a bytestring, assumes a hash primitive
mkHash :: ByteString -> Hash
mkHash = hash

-- | the leaf of a tree
mkLeaf :: ByteString -> MerkleTree
mkLeaf = MerkleTree $ mkHash $ "0" <> bs

-- | the node of a tree based off of two child nodes
mkNode :: MerkleTree -> MerkleTree -> MerkleTree
mkNode (MerkleTree (MkHash h1)) (MerkleTree (MkHash h2)) = MerkleTree $ mkHash $ "1" <> h1 <> h2

-- | create the tree to put from the offchain to the onchain as a commitment
mkTree :: Tree -> MerkleTree
mkTree (Leaf bs) = mkLeaf bs
mkTree (Node left right) = mkNode (mkTree left) (mkTree right)

data Path = Here
          | LeftPath MerkleTree MerkleTree Path 
          | RightPath MerkleTree MerkleTree Path

proveIn :: Path -> MerkleTree -> ByteString -> Bool
proveIn Here t bs = mkLeaf bs == t
proveIn (LeftPath left right p) t bs = mkNode left right == t && proveIn p left bs
proveIn (RightPath left right p) t bs = mkNode left right == t && proveIn p right bs


newtype Hash = MkHash ByteString
```

### Authenticity

We can prove that the data has come from a specific website
using zero-knowledge proofs if the connection was served over TLS.
