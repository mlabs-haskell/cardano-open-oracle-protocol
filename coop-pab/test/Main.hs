{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import BotPlutusInterface.Types (LogContext (ContractLog), LogLevel (Info))
import Coop.Pab (burnCerts, deployAuth, deployCoop, findOutsAtCertVWithCERT, findOutsAtOwnHolding, findOutsAtOwnHoldingAa, mintCert, mintCertRedeemers, testDataRoundtrip, testDataRoundtrip')
import Coop.Pab.Aux (DeployMode (DEPLOY_DEBUG), loadCoopPlutus, makeCollateralOuts, mintNft)
import Coop.Types (CertDatum (CertDatum), CoopPlutus (cp'certV, cp'mkNftMp))
import Data.Bifunctor (Bifunctor (second))
import Data.Bool (bool)
import Data.Default (def)
import Data.List.NonEmpty (NonEmpty)
import GHC.Natural (Natural)
import Ledger (Validator (Validator), interval)
import Ledger.Value (assetClass, currencySymbol, tokenName)
import Plutus.Contract (currentTime, logInfo, throwError, waitNSlots)
import Test.Plutip.Contract (TestWallets, assertExecutionWith, initAda, withContract)
import Test.Plutip.Internal.Types (ClusterEnv)
import Test.Plutip.LocalCluster (BpiWallet, withConfiguredCluster)
import Test.Plutip.Options (TraceOption (ShowBudgets, ShowTraceButOnlyContext))
import Test.Plutip.Predicate (shouldSucceed, shouldYield)
import Test.Tasty (DependencyType (AllFinish), TestName, TestTree, after, defaultMain)

main :: IO ()
main = do
  coopPlutus <- loadCoopPlutus DEPLOY_DEBUG
  defaultMain (tests coopPlutus)

slotsToWait :: Natural
slotsToWait = 40

testOpts :: [TraceOption]
testOpts = [ShowTraceButOnlyContext ContractLog Info, ShowBudgets]

tests :: CoopPlutus -> TestTree
tests coopPlutus =
  withConfiguredCluster
    def
    "coop-pab-tests"
    [ assertExecutionWith
        testOpts
        "datum-roundtrip"
        (initAda [100])
        ( withContract @String
            ( const $ do
                _ <- waitNSlots slotsToWait
                let validityInterval = interval 0 100_000
                    x = CertDatum "" validityInterval (assetClass (currencySymbol "") (tokenName ""))
                if testDataRoundtrip' x
                  then do
                    testDataRoundtrip (Validator . cp'certV $ coopPlutus) x
                  else do
                    throwError "Pure roundtrip failed"
            )
        )
        [shouldSucceed]
    , assertExecutionWith
        testOpts
        "mint-nft"
        (initAda [100])
        ( withContract @String
            ( const $ do
                _ <- waitNSlots slotsToWait
                (_, (nftAc, _)) <- mintNft (cp'mkNftMp coopPlutus) 1
                _ <- waitNSlots slotsToWait
                found <- findOutsAtOwnHolding nftAc
                return $ length found
            )
        )
        [shouldSucceed, shouldYield 1]
    , runAfter "mint-nft" $
        assertExecutionWith
          testOpts
          "deploy-authentication"
          (initAda [100])
          ( withContract @String
              ( const $ do
                  _ <- waitNSlots slotsToWait
                  _ <- deployAuth coopPlutus 3
                  waitNSlots slotsToWait
              )
          )
          [shouldSucceed]
    , runAfter "deploy-authentication" $
        assertExecutionWith
          testOpts
          "deploy-coop"
          (initAda [200])
          ( withContract @String
              ( const $ do
                  _ <- makeCollateralOuts 5 20_000_000
                  _ <- waitNSlots slotsToWait
                  coopDeployment <- deployCoop coopPlutus 3
                  _ <- waitNSlots slotsToWait
                  aaOuts <- findOutsAtOwnHoldingAa coopDeployment
                  return $ length aaOuts
              )
          )
          [shouldSucceed, shouldYield 1]
    , runAfter "deploy-coop" $
        assertExecutionWith
          testOpts
          "mint-cert"
          (initAda [200])
          ( withContract @String
              ( const $ do
                  _ <- makeCollateralOuts 5 20_000_000
                  _ <- waitNSlots slotsToWait
                  coopDeployment <- deployCoop coopPlutus 3
                  _ <- waitNSlots slotsToWait
                  (_, (certRedeemerAc, _)) <- mintCertRedeemers 100 coopPlutus
                  _ <- waitNSlots slotsToWait
                  aaOuts <- findOutsAtOwnHoldingAa coopDeployment
                  _ <- waitNSlots slotsToWait
                  now <- currentTime
                  let validityInterval = interval now (now + 100_000)
                  _ <- mintCert certRedeemerAc validityInterval aaOuts coopDeployment
                  _ <- waitNSlots slotsToWait
                  certOuts <- findOutsAtCertVWithCERT coopDeployment
                  logInfo $ "Found " <> (show . length $ certOuts) <> " $CERT outputs"
                  bool (throwError "There should be some $CERT inputs") (pure ()) $ not (null certOuts)
                  waitNSlots slotsToWait
              )
          )
          [shouldSucceed]
    , runAfter "mint-cert" $
        assertExecutionWith
          testOpts
          "burn-cert"
          (initAda [200])
          ( withContract @String
              ( const $ do
                  _ <- makeCollateralOuts 5 20_000_000
                  _ <- waitNSlots slotsToWait
                  coopDeployment <- deployCoop coopPlutus 3
                  _ <- waitNSlots slotsToWait
                  (_, (certRedeemerAc, _)) <- mintCertRedeemers 100 coopPlutus
                  _ <- waitNSlots slotsToWait
                  aaOuts <- findOutsAtOwnHoldingAa coopDeployment
                  _ <- waitNSlots slotsToWait
                  now <- currentTime
                  let validityInterval = interval now (now + 100_000)
                  _ <- mintCert certRedeemerAc validityInterval aaOuts coopDeployment
                  _ <- waitNSlots slotsToWait
                  certRedeemerOuts <- findOutsAtOwnHolding certRedeemerAc
                  certOuts <- findOutsAtCertVWithCERT coopDeployment
                  logInfo $ "Found " <> (show . length $ certOuts) <> " $CERT outputs"
                  bool (throwError "There should be some $CERT inputs") (pure ()) $ not (null certOuts)
                  _ <- burnCerts certOuts certRedeemerOuts coopDeployment
                  waitNSlots slotsToWait
              )
          )
          [shouldSucceed]
    ]

runAfter ::
  TestName ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
runAfter testName = second (fmap . after AllFinish $ '/' : testName ++ "/")
