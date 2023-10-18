module Coop.Plutus.Test (spec) where

import Plutarch.Prelude (ClosedTerm, PEq ((#==)), pconstant, pconstantData, (#))
import Test.Hspec (Expectation, Spec, describe, expectationFailure, runIO, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonEmptyList (getNonEmpty), Positive (getPositive), choose, forAll, generate)

import Codec.Serialise (deserialiseOrFail)
import Coop.Plutus (certV, exampleConsumer, fsV, mkAuthMp, mkCertMp, mkFsMp, pmustSpendAtLeastAa)
import Coop.Plutus.Aux (hashTxInputs, pmustBurnOwnSingletonValue, punit)
import Coop.Plutus.Test.Generators (distribute, genAaInputs, genCertRdmrAc, genCorrectAuthMpBurningCtx, genCorrectAuthMpMintingCtx, genCorrectCertMpBurningCtx, genCorrectCertMpMintingCtx, genCorrectCertVSpendingCtx, genCorrectConsumerCtx, genCorrectFsMpBurningCtx, genCorrectFsMpMintingCtx, genCorrectFsVSpendingCtx, genCorrectMustBurnOwnSingletonValueCtx, genCorruptAuthMpBurningCtx, genCorruptAuthMpMintingCtx, genCorruptCertMpBurningCtx, genCorruptCertMpMintingCtx, genCorruptCertVSpendingCtx, genCorruptFsMpBurningCtx, genCorruptFsMpMintingCtx, genCorruptFsVSpendingCtx, genCorruptMustBurnOwnSingletonValueCtx, mkScriptContext)
import Coop.Plutus.Types (PAuthMpParams, PCertMpParams)
import Coop.Types (AuthMpParams (AuthMpParams), AuthMpRedeemer (AuthMpBurn, AuthMpMint), AuthParams (AuthParams), CertMpParams (CertMpParams), CertMpRedeemer (CertMpBurn, CertMpMint), FsMpParams (FsMpParams), FsMpRedeemer (FsMpBurn, FsMpMint))
import Data.ByteString.Lazy qualified as LB
import Data.Foldable (Foldable (fold))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text, unpack)
import Plutarch (Config (Config, tracingMode), TracingMode (DoTracing), compile)
import Plutarch.Api.V1 (PCurrencySymbol)
import Plutarch.Builtin (PIsData (pdataImpl))
import Plutarch.Evaluate (evalScript)
import Plutarch.Test (pfails)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Scripts (applyArguments)
import PlutusLedgerApi.V1.Value (AssetClass, TokenName (TokenName), assetClass, currencySymbol)
import PlutusLedgerApi.V2 (Address, CurrencySymbol, Script, ScriptPurpose (Minting), ValidatorHash (ValidatorHash), dataToBuiltinData, toData)
import PlutusTx (Data)
import PlutusTx.Builtins.Class (stringToBuiltinByteString)

coopAc :: AssetClass
coopAc = assetClass (currencySymbol "$COOP CurrencySymbol") (TokenName "$COOP TokenName")

aaAc :: AssetClass
aaAc = assetClass (currencySymbol "$AA CurrencySymbol") (TokenName "$AA TokenName")

certCs :: CurrencySymbol
certCs = currencySymbol "CertMp hash"

authCs :: CurrencySymbol
authCs = currencySymbol "AuthMp hash"

fsCs :: CurrencySymbol
fsCs = currencySymbol "FsMp hash"

certVAddr :: Address
certVAddr = scriptHashAddress . ValidatorHash . stringToBuiltinByteString $ "@CertV hash"

fsVAddr :: Address
fsVAddr = scriptHashAddress . ValidatorHash . stringToBuiltinByteString $ "@FsV hash"

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

  describe "pmustBurnOwnSingletonValue" $ do
    prop "should-succeed" $ do
      forAll genCorrectMustBurnOwnSingletonValueCtx $
        \ctx ->
          psucceeds (pmustBurnOwnSingletonValue # pconstant ctx)
    prop "should-fail" $ do
      forAll genCorruptMustBurnOwnSingletonValueCtx $
        \ctx ->
          pfails (pmustBurnOwnSingletonValue # pconstant ctx)
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

  describe "FsMp" $ do
    describe "should-succeed" $ do
      prop "mint $FS" $
        let fsMpParams = FsMpParams coopAc fsVAddr (AuthParams authCs certCs)
            fsMp = applyArguments (comp mkFsMp) [toData fsMpParams]
         in forAll (genCorrectFsMpMintingCtx fsMpParams fsCs) $
              \ctx ->
                succeeds $ applyArguments fsMp [toData FsMpMint, toData ctx]
      prop "burn $FS" $
        let fsMpParams = FsMpParams coopAc fsVAddr (AuthParams authCs certCs)
            fsMp = applyArguments (comp mkFsMp) [toData fsMpParams]
         in forAll (genCorrectFsMpBurningCtx fsMpParams fsCs) $
              \ctx ->
                succeeds $ applyArguments fsMp [toData FsMpBurn, toData ctx]
    describe "should-fail" $ do
      prop "mint $FS" $
        let fsMpParams = FsMpParams coopAc fsVAddr (AuthParams authCs certCs)
            fsMp = applyArguments (comp mkFsMp) [toData fsMpParams]
         in forAll (genCorruptFsMpMintingCtx fsMpParams fsCs) $
              \ctx ->
                fails $ applyArguments fsMp [toData FsMpMint, toData ctx]
      prop "burn $FS" $
        let fsMpParams = FsMpParams coopAc fsVAddr (AuthParams authCs certCs)
            fsMp = applyArguments (comp mkFsMp) [toData fsMpParams]
         in forAll (genCorruptFsMpBurningCtx fsMpParams fsCs) $
              \ctx ->
                fails $ applyArguments fsMp [toData FsMpBurn, toData ctx]
  describe "@FsV" $ do
    describe "should-succeed" $ do
      prop "spend $FS" $
        forAll genCorrectFsVSpendingCtx $
          \ctx ->
            psucceeds
              ( fsV
                  # pconstant (toData ())
                  # pconstant (toData ())
                  # pconstant ctx
              )
    describe "should-fail" $ do
      prop "spend $FS" $
        forAll genCorruptFsVSpendingCtx $
          \ctx ->
            pfails
              ( fsV
                  # pconstant (toData ())
                  # pconstant (toData ())
                  # pconstant ctx
              )
  describe "@Consumer" $ do
    samplePd <- runIO $ readPlutusDataCbor "resources/sample.pd.cbor"
    describe "should-succeed" $ do
      prop "reference a fact statement" $
        forAll (genCorrectConsumerCtx fsCs (dataToBuiltinData samplePd)) $
          \ctx ->
            psucceeds
              ( exampleConsumer
                  # pconstant @PCurrencySymbol fsCs
                  # pconstant (toData ())
                  # pconstant (toData ())
                  # pconstant ctx
              )

_plog :: ClosedTerm a -> Expectation
_plog p = _ptraces' p id []

_ptraces' :: (Show b, Eq b) => ClosedTerm a -> ([Text] -> b) -> b -> Expectation
_ptraces' p traceMap traceMappedShouldBe =
  case evalScript $ comp p of
    (Left _, _, traceLog) -> traceMap traceLog `shouldBe` traceMappedShouldBe
    (Right _, _, traceLog) -> traceMap traceLog `shouldBe` traceMappedShouldBe

comp :: ClosedTerm a -> Script
comp t = either (error . unpack) id $ compile (Config {tracingMode = DoTracing}) t

passert :: ClosedTerm a -> Expectation
passert p = pshouldBe p punit

psucceeds :: ClosedTerm a -> Expectation
psucceeds = passert

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
  evalScript x `shouldBe` evalScript y

readPlutusDataCbor :: FilePath -> IO Data
readPlutusDataCbor fname = do
  cborBytes <- LB.readFile fname
  let errOrDecoded = deserialiseOrFail @Data cborBytes
  either (\err -> error $ "File " <> fname <> " can't be parsed into PlutusData CBOR: " <> show err) return errOrDecoded

-- | Asserts the term evaluates successfully without failing
succeeds :: Script -> Expectation
succeeds s =
  case evalScript s of
    (Left _, _, t) -> expectationFailure $ "Term failed to evaluate, here's the trace:\n" <> show t
    (Right _, _, _) -> pure ()

-- | Asserts the term evaluates without success
fails :: Script -> Expectation
fails s = do
  case evalScript s of
    (Left _, _, _) -> pure ()
    (Right _, _, t) -> expectationFailure $ "Term succeeded, here's the trace:\n" <> show t
