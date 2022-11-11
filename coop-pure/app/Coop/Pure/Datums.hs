module Coop.Pure.Datums where

-- TODO: rewrite this using `Data.Row`

data Arity
  = Primitive
  | Multiple

data UTXOData (arity :: Arity) (isSequential :: Bool) = MkUTXOData
  { isPrimitive :: Bool
  -- ^ specifies whether you have a primitive datum or a composition
  --   of them
  , typeId :: MkTypeId arity
  -- ^ function(s) from the URI of the Datum, see URI type
  --   to the NFT identifying the UTXOData
  , metadata :: MetaData
  , datum :: [(URIHash, Data)]
  -- ^ the actual data attached to the UTXOData
  , predecessors :: MkPredecessor arity isSequential
  -- ^ optional link back to predecessor nodes
  --   TODO: Is this necessary? Maybe it could be derived from the URI
  }

data MetaData = MkMetaData
  { validityInterval :: POSIXInterval
  -- ^ datum is valid from, until
  , timeToLive :: Extended POSIXTime
  -- ^ to gain back a part of the spent money for the locked
  --   UTXOs, onChainUntil >= validityInterval.upperBound
  }

-- NOTE: this can also be `UpperBound` to include Closure information

-- | The URI of a specific Datum
data URI = MkURI
  { streamID :: CurrencySymbol
  -- ^ the ID of a stream, e.g. the orcfax specific currency symbol
  , categorization :: ByteString
  -- ^ semantical category of the stream, e.g. "cardano/coin/prices"
  , dataUid :: ByteString
  -- ^ the UID specialising the instance of a category of `dataStreamName`
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

-- FIXME: import Plutus Ledger Types
data Value = forall a b. MkValue (Map a b)
data Data
data POSIXInterval
data Extended a
data POSIXTime
data CurrencySymbol
