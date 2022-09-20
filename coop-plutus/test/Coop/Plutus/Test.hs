module Coop.Plutus.Test (spec) where

import Plutarch.Prelude (ClosedTerm, PBool (PTrue), PEq ((#==)), pconstant, pconstantData, (#))
import Test.Hspec (Expectation, Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonEmptyList (getNonEmpty), Positive (getPositive), choose, forAll, generate)

import Coop.Plutus (mkCertMp, pmustSpendAtLeastAa)
import Coop.Plutus.Aux (hashTxInputs)
import Coop.Plutus.Test.Generators (distribute, genAaInputs, genCorrectCertMpMintingArgs, genCorruptCertMpMintingArgs)
import Coop.Plutus.Types (PCertMpParams)
import Coop.Types (CertMpParams (CertMpParams), CertMpRedeemer (CertMpMint))
import Data.ByteString (ByteString)
import Data.Foldable (Foldable (fold))
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text, unpack)
import Plutarch (Config (Config, tracingMode), TracingMode (DetTracing), compile, pcon, printScript)
import Plutarch.Builtin (PIsData (pdataImpl))
import Plutarch.Evaluate (evalScript)
import Plutarch.Test (pfails, psucceeds)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (AssetClass, TokenName (TokenName), assetClass, currencySymbol)
import PlutusLedgerApi.V2 (Address, BuiltinByteString, CurrencySymbol, PubKeyHash, Script, ScriptContext (ScriptContext), ScriptPurpose (Minting), TxInInfo (TxInInfo), TxInfo (TxInfo, txInfoDCert, txInfoData, txInfoFee, txInfoId, txInfoInputs, txInfoMint, txInfoOutputs, txInfoRedeemers, txInfoReferenceInputs, txInfoSignatories, txInfoValidRange, txInfoWdrl), TxOut, ValidatorHash (ValidatorHash), Value, always, toBuiltin)
import PlutusTx.AssocMap qualified as AssocMap

aaAc :: AssetClass
aaAc = assetClass (currencySymbol "$AA CurrencySymbol") (TokenName "$AA TokenName")

certCs :: CurrencySymbol
certCs = currencySymbol "CertMp hash"

certVAddr :: Address
certVAddr = scriptHashAddress . ValidatorHash $ toBuiltin @ByteString @BuiltinByteString "@CertV hash"

mkScriptContext :: ScriptPurpose -> [TxInInfo] -> [TxInInfo] -> Value -> [TxOut] -> [PubKeyHash] -> ScriptContext
mkScriptContext purpose ins refs mints outs sigs =
  ScriptContext (mkTxInfo ins refs mints outs sigs) purpose

mkTxInfo :: [TxInInfo] -> [TxInInfo] -> Value -> [TxOut] -> [PubKeyHash] -> TxInfo
mkTxInfo ins refs mints outs sigs =
  TxInfo
    { txInfoFee = mempty
    , txInfoDCert = mempty
    , txInfoWdrl = AssocMap.empty
    , txInfoValidRange = always
    , txInfoData = AssocMap.empty
    , txInfoId = ""
    , txInfoRedeemers = AssocMap.empty
    , txInfoInputs = sortOn (\(TxInInfo i _) -> i) ins
    , txInfoReferenceInputs = sortOn (\(TxInInfo i _) -> i) refs
    , txInfoMint = mints
    , txInfoOutputs = outs
    , txInfoSignatories = sigs
    }

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

  describe "CertMp" $ do
    describe "should-succeed" $ do
      prop "mint $CERT" $
        forAll (choose (1, 10)) $
          \aaQ ->
            let certMpParams = CertMpParams aaAc aaQ certVAddr
             in forAll (genCorrectCertMpMintingArgs certMpParams certCs) $
                  \ctx ->
                    psucceeds
                      ( mkCertMp
                          # pconstantData @PCertMpParams certMpParams
                          # pdataImpl (pconstant CertMpMint)
                          # pconstant ctx
                      )
    describe "should-fail" $ do
      prop "mint $CERT" $
        forAll (choose (1, 10)) $
          \aaQ ->
            let certMpParams = CertMpParams aaAc aaQ certVAddr
             in forAll (genCorruptCertMpMintingArgs certMpParams certCs) $
                  \ctx ->
                    pfails
                      ( mkCertMp
                          # pconstantData @PCertMpParams certMpParams
                          # pdataImpl (pconstant CertMpMint)
                          # pconstant ctx
                      )
  describe "@CertV" $ do return ()
  describe "AuthMp" $ do return ()
  describe "FsMp" $ do return ()
  describe "@FsV" $ do return ()

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
