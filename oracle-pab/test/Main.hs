module Main (main) where

import Cardano.Oracle.Pab ()
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "oracle-pab-tests"
    []
