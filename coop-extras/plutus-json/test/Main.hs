module Main (main) where

import PlutusJson (jsonToPlutusData, plutusDataToJson)
import PlutusTx (Data)
import Test.Hspec (Spec, describe, hspec, shouldBe)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "plutus-json-tests" $ do
    prop "Json -> PlutusData -> Json should yield the same object" $ \aes -> do
      let pd = jsonToPlutusData aes
      aes' <- plutusDataToJson pd
      aes `shouldBe` aes'
