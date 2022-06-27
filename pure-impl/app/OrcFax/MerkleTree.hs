{-# OPTIONS_GHC -Wno-orphans #-}

module OrcFax.MerkleTree (proveIn, prop_proof_holds_for_id_in_tree, prop_proof_does_not_hold_for_id_not_in_tree, prop_in_oracle_tree, inLeaf, wrongData, wrongId, sampleOracle) where

import Crypto.Hash.SHA256 (hash)
import Data.Aeson (ToJSON, encode)
import Data.ByteString (pack)
import GHC.Records (HasField (getField))
import Test.QuickCheck (Arbitrary (arbitrary), Property, elements, suchThat, (===))
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary (GenericArbitrary))

-- TODO:
-- - more documentation
-- - write QuickCheck properties to test whether this actuall works
-- - use ledger types, write actual implementation

--------------------------------- The Merkle Tree -------------------------

-- | a generic tree type
data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving stock (Eq, Generic, Show, Functor)
  deriving (Arbitrary) via GenericArbitrary (Tree a)

-- | a merkle tree, this corresponds to the top node of the tree
newtype MerkleTree = MerkleTree Hash
  deriving stock (Eq, Generic)
  deriving (Arbitrary) via GenericArbitrary MerkleTree

-- | a sha256 hash
newtype Hash = MkHash ByteString
  deriving stock (Eq, Generic)
  deriving (Arbitrary) via (GenericArbitrary Hash)

-- | smart constructor for a Hash
mkHash :: ByteString -> Hash
mkHash = MkHash . hash

-- | make a leaf of a MerkleTree
mkLeaf :: ByteString -> MerkleTree
mkLeaf bs = MerkleTree $ mkHash $ "0" <> bs

-- | make a node of a MerkleTree
mkNode :: MerkleTree -> MerkleTree -> MerkleTree
mkNode (MerkleTree (MkHash h1)) (MerkleTree (MkHash h2)) = MerkleTree $ mkHash $ "1" <> h1 <> h2

-- | make a MerkleTree from a Tree by encoding its JSON into ByteStrings
mkTree :: ToJSON a => Tree a -> MerkleTree
mkTree (Leaf odata) = mkLeaf $ toStrict $ encode odata
mkTree (Node left right) = mkNode (mkTree left) (mkTree right)

-- | flattens a tree to a list
flattenTree :: Tree a -> [a]
flattenTree (Leaf a) = [a]
flattenTree (Node a b) = flattenTree a <> flattenTree b

instance Foldable Tree where
  foldr f a t = foldr f a $ flattenTree t

{- | the Path into the MerkleTree, this has to be provided with the
   redeemer
-}
data Path
  = Here
  | LeftPath MerkleTree MerkleTree Path
  | RightPath MerkleTree MerkleTree Path

{- | the proof that some redeemer datum is part of the commitment of
   an oracle. We can prove this given a path into the tree, the topmost node
   and the atual datum
-}
proveIn :: Path -> MerkleTree -> ByteString -> Bool
proveIn Here t bs = mkLeaf bs == t
proveIn (LeftPath left right p) t bs = mkNode left right == t && proveIn p left bs
proveIn (RightPath left right p) t bs = mkNode left right == t && proveIn p right bs

--------------------------------- The Offchain part -----------------------

{- | the registry containing the Tree and the corresponding MerkleTree
   this would be made available at any time at some online API. The MerkleTree
   corresponds to the reference input provided by the oracle
-}
newtype Registry rec = MkRegistry {unRegistry :: (Tree rec, MerkleTree)}

-- | make an oracleentry from the current OracleData
mkOracleRegistry :: Tree OracleData -> Registry OracleData
mkOracleRegistry t = MkRegistry (t, mkTree t)

{- | lookup the registry, this is the offchain function to obtain the mapping from a path to a MerkleTree
   at the api, the user would request the path for a datum id and get back the path into the tree that
   they can provide with their redeemer as well as the Datum
-}
lookupRegistry :: forall rec. (HasField "i" rec Id, ToJSON rec) => Id -> Registry rec -> Either Text (Path, rec)
lookupRegistry i = note "lookup in oracle tree failed" . lookupOracleTree i . fst . unRegistry
  where
    lookupOracleTree :: (HasField "i" rec Id, ToJSON rec) => Id -> Tree rec -> Maybe (Path, rec)
    lookupOracleTree id' (Leaf odata)
      | getField @"i" odata == id' = Just (Here, odata)
      | otherwise = Nothing
    lookupOracleTree id'' (Node t0 t1) =
      let mt0 = mkTree t0
          mt1 = mkTree t1
       in (first (LeftPath mt0 mt1) <$> lookupOracleTree id'' t0) <|> (first (RightPath mt0 mt1) <$> lookupOracleTree id'' t1)

obtainRegistryHash :: Registry rec -> MerkleTree
obtainRegistryHash = snd . unRegistry

newtype Id = MkId Int
  deriving stock (Generic, Show)
  deriving newtype (Arbitrary, Eq, Ord)
  deriving anyclass (ToJSON)

data OracleData = MkOracleData
  { i :: Id
  , name :: String
  , contents :: String
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON)
  deriving (Arbitrary) via (GenericArbitrary OracleData)

--------------------------------- Example ---------------------------------

prop_in_oracle_tree :: Id -> Tree OracleData -> Either Text Bool
prop_in_oracle_tree datumID tree = do
  let sampleRegistry :: Registry OracleData
      sampleRegistry = mkOracleRegistry tree

      -- this corresponds to the hash lookup the script would do to
      -- verify the redeemer is correct
      onchainHashLookup :: MerkleTree
      onchainHashLookup = obtainRegistryHash sampleRegistry

  -- corresponds to the offchain lookup of the user of the oracle
  (path, datum) <- lookupRegistry datumID sampleRegistry
  let -- encode the datum to put onchain
      -- if we were to put a different datum here, e.g. the wrongData
      -- datum, we would get a failure (in all cases)
      encodedDatum :: ByteString
      encodedDatum = toStrict $ encode datum
  pure $ proveIn path onchainHashLookup encodedDatum

newtype IdAndOracleTree = MkIdInOracleTree (Id, Tree OracleData)
  deriving stock (Show)
newtype IdNotInOracleTree = MkIdNotInOracleTree (Id, Tree OracleData)
  deriving stock (Show)

instance Arbitrary IdAndOracleTree where
  arbitrary = do
    oracleTree :: Tree OracleData <- arbitrary
    oracleId :: Id <- i <$> elements (flattenTree oracleTree)
    pure $ MkIdInOracleTree (oracleId, oracleTree)

instance Arbitrary IdNotInOracleTree where
  arbitrary = do
    oracleTree :: Tree OracleData <- arbitrary
    oracleId :: Id <- arbitrary `suchThat` (\e -> not $ e `elem` fmap i oracleTree)
    pure $ MkIdNotInOracleTree (oracleId, oracleTree)

prop_proof_holds_for_id_in_tree :: IdAndOracleTree -> Property
prop_proof_holds_for_id_in_tree (MkIdInOracleTree (id', tree)) =
  prop_in_oracle_tree id' tree === Right True

prop_proof_does_not_hold_for_id_not_in_tree :: IdNotInOracleTree -> Property
prop_proof_does_not_hold_for_id_not_in_tree (MkIdNotInOracleTree (id', tree)) =
  prop_in_oracle_tree id' tree === Left "lookup in oracle tree failed"

inLeaf :: OracleData
inLeaf =
  MkOracleData
    { i = MkId 0
    , name = "ada-to-euro"
    , contents = "1:1"
    }

wrongData :: OracleData
wrongData =
  MkOracleData
    { i = MkId 0
    , name = "ada-to-euro"
    , contents = "1:2"
    }

wrongId :: OracleData
wrongId =
  MkOracleData
    { i = MkId 42
    , name = "ada-to-euro"
    , contents = "1:1"
    }

sampleOracle :: Tree OracleData
sampleOracle =
  Node
    ( Node
        ( Leaf
            ( MkOracleData
                { i = MkId 0
                , name = "ada-to-euro"
                , contents = "1:1"
                }
            )
        )
        ( Leaf
            ( MkOracleData
                { i = MkId 1
                , name = "ada-to-dollar"
                , contents = "0.9:1"
                }
            )
        )
    )
    ( Node
        ( Leaf
            ( MkOracleData
                { i = MkId 2
                , name = "ada-to-ether"
                , contents = "1000:1"
                }
            )
        )
        ( Leaf
            ( MkOracleData
                { i = MkId 3
                , name = "ada-to-bc"
                , contents = "20000:1"
                }
            )
        )
    )

instance Arbitrary ByteString where
  arbitrary = pack <$> arbitrary

note :: IsString s => s -> Maybe a -> Either s a
note _ (Just a) = Right a
note s Nothing = Left s
