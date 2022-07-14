# Onchain Database

## Description

1. Data structured similarly to `Data.Row`, see
   [row-types](https://hackage.haskell.org/package/row-types)

2. Commitment scheme is possible

3. [Product Nodes](#product-nodes)

4. Tabular form higher order nodes

### Graph

```text
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

- any user can combine information from any `n` nodes and manipulate it using a
  function as long as they can prove that the combination maintains trust
  according to the specification of trust given by the authority. The result is
  provided in a new Datum.

- `f : [Data] -> ([Data] -> Data) -> MintingPolicy -> (CurrencySymbol, Data)`
  (rough sketch, this doesn't correspond with the actual implementation)

- can also be used by the actual authority to collapse datums that are similar
  into one maintaining reference to old nodes

```text

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

*Note: making sure that the function we use to combine datums computes a datum
that can be trusted is non-trivial in the general case*

### Interactions/ Shape of an onchain datum

1. Expiration and TTL

    1. validity interval: the interval in which the publisher deems a datum to be
  valid in
    2. time to live: must be at least upperbound of validity interval;
    corresponds to the time from when on the publisher will be garbage
    collecting the datum; useful for gaining back funds.

2. Commitment Scheme

    1. This can be the scheme proposed [beneath](#using-a-commitment-scheme) or
       basically any other commitment scheme, the designs are not mutually
       exclusive
3. Naming scheme in the data registry; the datum-`URI`

    1. two main requirements for the URI: representative (searchable), unique
      `<currency_symbol_per_provider>/<categorization>/<uid>`
    2. `currency_symbol_per_provider`: MintingPolicy for authority and specific
       information stream (or MintingPolicy provided by authority that allows
       for combination of datums without losing trust which is bound to rules
       defined by authority)
    3. example: "`<OrcFax-specify cursym>/nasdaq/goog/price/<uid identifying the
      query done by the oracle>`" (this corresponds to the `URI`-field in `UTXOData`)

> TODO: fill in the missing fields of the sample datatype and everything that
> might be in it

## An onchain data registry

### Draft of a possible Onchain representation of the datum

```haskell

data Arity = Primitive | Multiple

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

*To avoid unnecessary onchain data the `UTXOData` can be specialized to not
contain a predecessor field (depending on the use-case a datum might not depend
on a previous datum)*
