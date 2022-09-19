module Main (main) where

import Test.Hspec (Spec, describe, hspec)

import Coop.Test.Plutus (spec)

main :: IO ()
main = do
  hspec tests

tests :: Spec
tests = do
  describe "COOP Plutus spec" spec
