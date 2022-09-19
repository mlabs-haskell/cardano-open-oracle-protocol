module Coop.Test.Plutus (spec) where

import Plutarch.Api.V2 ()
import Plutarch.Prelude ()
import Plutarch.Test ()
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck ()
import Test.QuickCheck ()

import PlutusLedgerApi.V1.Scripts (applyArguments)
import PlutusLedgerApi.V2 ()

spec :: Spec
spec = do
  describe "mint-cert-spec" $ do return ()
  describe "burn-cert-spec" $ do return ()
  describe "mint-auth-spec" $ do return ()
  describe "burn-auth-spec" $ do return ()
  describe "mint-fs-spec" $ do return ()
  describe "burn-fs-spec" $ do return ()
