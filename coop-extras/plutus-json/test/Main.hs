module Main (main) where

import PlutusJson
import Test.Hspec (Spec, describe, hspec)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "icebreaker" $ do
    prop "icebreaker" $ \x y -> (x :: Integer) + y == y + x
