module Main where

import Cardano.Oracle.Pure.MerkleTree qualified as MTree
import Test.QuickCheck (property, quickCheck, withMaxSuccess)

main :: IO ()
main =
  traverse_
    (quickCheck . withMaxSuccess 400)
    [ property MTree.prop_proof_does_not_hold_for_id_not_in_tree
    , property MTree.prop_proof_holds_for_id_in_tree
    ]
