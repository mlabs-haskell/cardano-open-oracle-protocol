module Coop.Plutus.Test (spec) where

import Plutarch.Prelude (ClosedTerm, PBool (PTrue), PEq ((#==)), pconstant, pconstantData, (#))
import Test.Hspec (Expectation, Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonEmptyList (getNonEmpty), Positive (getPositive), choose, forAll, generate)

import Coop.Plutus (certV, mkAuthMp, mkCertMp, pmustSpendAtLeastAa)
import Coop.Plutus.Aux (hashTxInputs, pmustSinkhole)
import Coop.Plutus.Test.Generators (distribute, genAaInputs, genCertRdmrAc, genCorrectAuthMpBurningCtx, genCorrectAuthMpMintingCtx, genCorrectCertMpBurningCtx, genCorrectCertMpMintingCtx, genCorrectCertVSpendingCtx, genCorrectMustSinkholeCtx, genCorruptAuthMpBurningCtx, genCorruptAuthMpMintingCtx, genCorruptCertMpBurningCtx, genCorruptCertMpMintingCtx, genCorruptCertVSpendingCtx, genCorruptMustSinkholeCtx, mkScriptContext)
import Coop.Plutus.Types (PAuthMpParams, PCertMpParams)
import Coop.Types (AuthMpParams (AuthMpParams), AuthMpRedeemer (AuthMpBurn, AuthMpMint), CertMpParams (CertMpParams), CertMpRedeemer (CertMpBurn, CertMpMint))
import Data.Foldable (Foldable (fold))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text, unpack)
import Plutarch (Config (Config, tracingMode), TracingMode (DetTracing), compile, pcon, printScript)
import Plutarch.Builtin (PIsData (pdataImpl))
import Plutarch.Evaluate (evalScript)
import Plutarch.Test (pfails, psucceeds)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (AssetClass, TokenName (TokenName), assetClass, currencySymbol)
import PlutusLedgerApi.V2 (Address, CurrencySymbol, Script, ScriptPurpose (Minting), ValidatorHash (ValidatorHash), toData)
import PlutusTx.Builtins.Class (stringToBuiltinByteString)

aaAc :: AssetClass
aaAc = assetClass (currencySymbol "$AA CurrencySymbol") (TokenName "$AA TokenName")

certCs :: CurrencySymbol
certCs = currencySymbol "CertMp hash"

authCs :: CurrencySymbol
authCs = currencySymbol "AuthMp hash"

certVAddr :: Address
certVAddr = scriptHashAddress . ValidatorHash . stringToBuiltinByteString $ "@CertV hash"

spec :: Spec
spec = do
  describe "test-aux" $ do
    prop "distribute-distributes-all" $ \(numbers :: [Int], chars :: NonEmptyList Char) -> do
      let keys = Set.fromList . getNonEmpty $ chars
      distributed <- generate $ distribute numbers keys
      (sum . fold $ distributed) `shouldBe` sum numbers
    prop "distribute-distributes-to-all" $ \(numbers :: [Int], chars :: NonEmptyList Char) -> do
      let keys = Set.fromList . getNonEmpty $ chars
      distributed <- generate $ distribute numbers keys
      Map.keysSet distributed `shouldBe` keys
  describe "pmustSpendAtLeastAa" $ do
    describe "should-succeed" $ do
      prop "hash-all-aa-inputs" $ -- TODO: Add a Plutip test for this
        forAll (choose (1, 10)) $
          \aaQ ->
            forAll (genAaInputs aaAc aaQ) $ \aaIns -> do
              let ctx = mkScriptContext (Minting certCs) aaIns [] mempty [] []
              passert
                (pmustSpendAtLeastAa # pconstant ctx # pconstant aaAc # 1 #== pconstant (hashTxInputs aaIns))
    describe "should-fail" $ do
      let ctx = mkScriptContext (Minting certCs) [] [] mempty [] []
      prop "must-have-non-zero-aa-inputs" $ \(aaQRequired :: Positive Integer) -> pfails (pmustSpendAtLeastAa # pconstant ctx # pconstant aaAc # pconstant (getPositive aaQRequired))

  describe "pmustSinkhole" $ do
    prop "should-succeed" $ do
      forAll genCorrectMustSinkholeCtx $
        \ctx ->
          psucceeds (pmustSinkhole # pconstant ctx)
    prop "should-fail" $ do
      forAll genCorruptMustSinkholeCtx $
        \ctx ->
          pfails (pmustSinkhole # pconstant ctx)
  describe "CertMp" $ do
    describe "should-succeed" $ do
      prop "mint $CERT" $
        forAll (choose (1, 10)) $
          \aaQ ->
            let certMpParams = CertMpParams aaAc aaQ certVAddr
             in forAll (genCorrectCertMpMintingCtx certMpParams certCs) $
                  \ctx ->
                    psucceeds
                      ( mkCertMp
                          # pconstantData @PCertMpParams certMpParams
                          # pdataImpl (pconstant CertMpMint)
                          # pconstant ctx
                      )
      prop "burn $CERT" $
        forAll (choose (1, 10)) $
          \aaQ ->
            let certMpParams = CertMpParams aaAc aaQ certVAddr
             in forAll (genCertRdmrAc >>= genCorrectCertMpBurningCtx certMpParams certCs) $
                  \ctx -> do
                    psucceeds
                      ( mkCertMp
                          # pconstantData @PCertMpParams certMpParams
                          # pdataImpl (pconstant CertMpBurn)
                          # pconstant ctx
                      )
    describe "should-fail" $ do
      prop "mint $CERT" $
        forAll (choose (1, 10)) $
          \aaQ ->
            let certMpParams = CertMpParams aaAc aaQ certVAddr
             in forAll (genCorruptCertMpMintingCtx certMpParams certCs) $
                  \ctx ->
                    pfails
                      ( mkCertMp
                          # pconstantData @PCertMpParams certMpParams
                          # pdataImpl (pconstant CertMpMint)
                          # pconstant ctx
                      )
      prop "burn $CERT" $
        forAll (choose (1, 10)) $
          \aaQ ->
            let certMpParams = CertMpParams aaAc aaQ certVAddr
             in forAll (genCertRdmrAc >>= genCorruptCertMpBurningCtx certMpParams certCs) $
                  \ctx ->
                    pfails
                      ( mkCertMp
                          # pconstantData @PCertMpParams certMpParams
                          # pdataImpl (pconstant CertMpBurn)
                          # pconstant ctx
                      )
  describe "@CertV" $ do
    describe "should-succeed" $ do
      prop "spend $CERT" $
        forAll (genCorrectCertVSpendingCtx certCs certVAddr) $
          \ctx ->
            psucceeds
              ( certV
                  # pconstant (toData ())
                  # pconstant (toData ())
                  # pconstant ctx
              )
    describe "should-fail" $ do
      prop "spend $CERT" $
        forAll (genCorruptCertVSpendingCtx certCs certVAddr) $
          \ctx ->
            pfails
              ( certV
                  # pconstant (toData ())
                  # pconstant (toData ())
                  # pconstant ctx
              )
  describe "AuthMp" $ do
    describe "should-succeed" $ do
      prop "mint $AUTH" $
        forAll (choose (1, 10)) $
          \aaQ ->
            let authMpParams = AuthMpParams aaAc aaQ
             in forAll (genCorrectAuthMpMintingCtx authMpParams authCs) $
                  \ctx ->
                    psucceeds
                      ( mkAuthMp
                          # pconstantData @PAuthMpParams authMpParams
                          # pdataImpl (pconstant AuthMpMint)
                          # pconstant ctx
                      )
      prop "burn $AUTH" $
        forAll (choose (1, 10)) $
          \aaQ ->
            let authMpParams = AuthMpParams aaAc aaQ
             in forAll (genCorrectAuthMpBurningCtx authCs) $
                  \ctx -> do
                    psucceeds
                      ( mkAuthMp
                          # pconstantData @PAuthMpParams authMpParams
                          # pdataImpl (pconstant AuthMpBurn)
                          # pconstant ctx
                      )
    describe "should-fail" $ do
      prop "mint $AUTH" $
        forAll (choose (1, 10)) $
          \aaQ ->
            let authMpParams = AuthMpParams aaAc aaQ
             in forAll (genCorruptAuthMpMintingCtx authMpParams authCs) $
                  \ctx ->
                    pfails
                      ( mkAuthMp
                          # pconstantData @PAuthMpParams authMpParams
                          # pdataImpl (pconstant AuthMpMint)
                          # pconstant ctx
                      )
      prop "burn $AUTH" $
        forAll (choose (1, 10)) $
          \aaQ ->
            let authMpParams = AuthMpParams aaAc aaQ
             in forAll (genCorruptAuthMpBurningCtx authCs) $
                  \ctx -> do
                    pfails
                      ( mkAuthMp
                          # pconstantData @PAuthMpParams authMpParams
                          # pdataImpl (pconstant AuthMpBurn)
                          # pconstant ctx
                      )

  describe "FsMp" $ do return ()
  describe "@FsV" $ do return ()

_plog :: ClosedTerm a -> Expectation
_plog p = _ptraces' p id []

_ptraces' :: (Show b, Eq b) => ClosedTerm a -> ([Text] -> b) -> b -> Expectation
_ptraces' p traceMap traceMappedShouldBe =
  case evalScript $ comp p of
    (Left _, _, traceLog) -> traceMap traceLog `shouldBe` traceMappedShouldBe
    (Right _, _, traceLog) -> traceMap traceLog `shouldBe` traceMappedShouldBe

comp :: ClosedTerm a -> Script
comp t = either (error . unpack) id $ compile (Config {tracingMode = DetTracing}) t

passert :: ClosedTerm a -> Expectation
passert p = pshouldBe p (pcon PTrue)

pshouldBe :: ClosedTerm a -> ClosedTerm b -> Expectation
pshouldBe x y = do
  p1 <- eval $ comp x
  p2 <- eval $ comp y
  pscriptShouldBe p1 p2
  where
    eval :: Script -> IO Script
    eval s = case evalScript s of
      (Left e, _, trace) -> fail $ "Script evaluation failed: " <> show e <> " with trace: " <> show trace
      (Right x', _, _) -> pure x'

{- |
  Like `pshouldBe` but on `Script`
-}
pscriptShouldBe :: Script -> Script -> Expectation
pscriptShouldBe x y =
  printScript x `shouldBe` printScript y
