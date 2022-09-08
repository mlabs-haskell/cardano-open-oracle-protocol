{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Aux (runAfter, withSuccessContract)
import BotPlutusInterface.Types (LogContext (ContractLog), LogLevel (Info))
import Control.Monad.Reader (ReaderT)
import Coop.Pab (burnAuths, burnCerts, deployCoop, findOutsAtCertVWithCERT, findOutsAtHoldingAa, mintCertRedeemers, mkMintAuthTrx, mkMintCertTrx)
import Coop.Pab.Aux (DeployMode (DEPLOY_DEBUG), ciValueOf, findOutsAt', findOutsAtHolding, findOutsAtHolding', loadCoopPlutus, makeCollateralOuts, mkMintNftTrx, submitTrx)
import Coop.Types (AuthDeployment (ad'authorityAc, ad'certV), CoopDeployment (cd'auth, cd'coopAc), CoopPlutus (cp'mkNftMp))
import Data.Bool (bool)
import Data.Default (def)
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import GHC.Natural (Natural)
import Ledger (interval)
import Ledger.Value (AssetClass)
import Plutus.Contract (currentTime, logInfo, ownFirstPaymentPubKeyHash, throwError, waitNSlots)
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import Test.Plutip.Contract (assertExecutionWith, initAda, withContract, withContractAs)
import Test.Plutip.Internal.Types (ClusterEnv)
import Test.Plutip.LocalCluster (BpiWallet, withConfiguredCluster)
import Test.Plutip.Options (TraceOption (ShowBudgets, ShowTraceButOnlyContext))
import Test.Plutip.Predicate (shouldSucceed, shouldYield)
import Test.Tasty (TestTree, defaultMain)

main :: IO ()
main = do
  coopPlutus <- loadCoopPlutus DEPLOY_DEBUG
  defaultMain (tests coopPlutus)

slotsToWait :: Natural
slotsToWait = 100

testOpts :: [TraceOption]
testOpts = [ShowTraceButOnlyContext ContractLog Info, ShowBudgets]

tests :: CoopPlutus -> TestTree
tests coopPlutus =
  withConfiguredCluster
    def
    "coop-pab-tests"
    [ assertExecutionWith
        testOpts
        "mint-nft"
        (initAda [100] <> initAda [100])
        ( withContract @String
            ( \[nftWallet] -> do
                self <- ownFirstPaymentPubKeyHash
                outs <- findOutsAt' @Void self (\_ _ -> True)
                let (trx, nftAc) = mkMintNftTrx self nftWallet (head . Map.toList $ outs) (cp'mkNftMp coopPlutus) 1
                _ <- submitTrx @Void trx
                _ <- waitNSlots slotsToWait
                found <- findOutsAtHolding' nftWallet nftAc
                return $ length found
            )
        )
        [shouldSucceed, shouldYield 1]
    , runAfter "mint-nft" $
        assertExecutionWith
          testOpts
          "deploy-coop"
          (initAda [200] <> initAda [200])
          ( withContract @String
              ( \[aaWallet] -> do
                  logInfo @String "Running as godWallet"
                  self <- ownFirstPaymentPubKeyHash
                  _ <- makeCollateralOuts self 5 20_000_000
                  _ <- waitNSlots slotsToWait
                  coopDeployment <- deployCoop coopPlutus self aaWallet 3
                  _ <- waitNSlots slotsToWait
                  let aaAc = ad'authorityAc . cd'auth $ coopDeployment
                      coopAc = cd'coopAc coopDeployment
                  aaOuts <- findOutsAtHolding' aaWallet aaAc
                  coopOuts <- findOutsAtHolding' self coopAc
                  return $
                    [ciValueOf aaAc out | out <- toList aaOuts]
                      <> [ciValueOf coopAc out | out <- toList coopOuts]
              )
          )
          [shouldSucceed, shouldYield [3, 1]]
    , runAfter "deploy-coop" $
        assertExecutionWith
          testOpts
          "mint-cert"
          (initAda [200] <> initAda [200] <> initAda [200])
          ( do
              (coopDeployment, certRedeemerAc) <- godDeploysCoop coopPlutus

              withContractAs @String
                1
                ( \_ -> do
                    logInfo @String "Running as aaWallet"
                    self <- ownFirstPaymentPubKeyHash
                    aaOuts <- findOutsAtHoldingAa self coopDeployment
                    now <- currentTime
                    let validityInterval = interval now (now + 100_000)
                        (mintCertTrx, certAc) = mkMintCertTrx coopDeployment self certRedeemerAc validityInterval aaOuts
                    _ <- submitTrx @Void mintCertTrx
                    _ <- waitNSlots slotsToWait
                    certOuts <- findOutsAtHolding (mkValidatorAddress . ad'certV . cd'auth $ coopDeployment) certAc
                    return [ciValueOf certAc out | out <- toList certOuts]
                )
          )
          [shouldSucceed, shouldYield [1]]
    , runAfter "mint-cert" $
        assertExecutionWith
          testOpts
          "burn-cert"
          (initAda [200] <> initAda [200] <> initAda [200])
          ( do
              (coopDeployment, certRedeemerAc) <- godDeploysCoop coopPlutus

              certAc <-
                withSuccessContract @String
                  1
                  ( \[_, _] -> do
                      logInfo @String "Running as aaWallet"
                      self <- ownFirstPaymentPubKeyHash
                      aaOuts <- findOutsAtHoldingAa self coopDeployment
                      now <- currentTime
                      let validityInterval = interval now (now + 100_000)
                          (mintCertTrx, certAc) = mkMintCertTrx coopDeployment self certRedeemerAc validityInterval aaOuts
                      _ <- submitTrx @Void mintCertTrx
                      _ <- waitNSlots slotsToWait
                      return certAc
                  )

              withContractAs @String
                2
                ( \[_, _] -> do
                    logInfo @String "Running as certRedeemerWallet"
                    self <- ownFirstPaymentPubKeyHash
                    certRedeemerOuts <- findOutsAtHolding' self certRedeemerAc
                    certOuts <- findOutsAtHolding (mkValidatorAddress . ad'certV . cd'auth $ coopDeployment) certAc
                    logInfo $ "Found " <> (show . length $ certOuts) <> " $CERT outputs"
                    bool
                      (throwError "There should be some $CERT inputs")
                      (logInfo @String $ "Found " <> (show . length $ certOuts) <> " $CERT inputs")
                      $ not (null certOuts)
                    _ <- burnCerts coopDeployment self certOuts certRedeemerOuts
                    waitNSlots slotsToWait
                )
          )
          [shouldSucceed]
    , runAfter "burn-cert" $
        assertExecutionWith
          testOpts
          "mint-auth"
          (initAda [200] <> initAda [200] <> initAda [200] <> initAda [200] <> initAda [200]) -- TODO: Make this more explicit somehow
          ( do
              (coopDeployment, _) <- godDeploysCoop coopPlutus

              withContractAs @String
                1
                ( \[_, _, authWalletGeorge, authWalletPeter] -> do
                    logInfo @String "Running as aaWallet"
                    self <- ownFirstPaymentPubKeyHash
                    aaOuts <- findOutsAtHoldingAa self coopDeployment
                    let (authMintTrx, authAc) = mkMintAuthTrx coopDeployment self [authWalletGeorge, authWalletPeter] 10 aaOuts
                    _ <- submitTrx @Void authMintTrx
                    _ <- waitNSlots slotsToWait
                    georgesOuts <- findOutsAtHolding' authWalletGeorge authAc
                    petersOuts <- findOutsAtHolding' authWalletPeter authAc
                    return $
                      [ciValueOf authAc out | out <- toList georgesOuts]
                        <> [ciValueOf authAc out | out <- toList petersOuts]
                )
          )
          [shouldSucceed, shouldYield [10, 10]]
    , runAfter "mint-auth" $
        assertExecutionWith @_ @Text
          testOpts
          "burn-auth"
          (initAda [200] <> initAda [200] <> initAda [200] <> initAda [200] <> initAda [200]) -- TODO: Make this more explicit somehow
          ( do
              (coopDeployment, _) <- godDeploysCoop coopPlutus

              authAc <-
                withSuccessContract @String
                  1
                  ( \[_, _, authWalletGeorge, authWalletPeter] -> do
                      logInfo @String "Running as aaWallet"
                      self <- ownFirstPaymentPubKeyHash
                      aaOuts <- findOutsAtHoldingAa self coopDeployment
                      let (authMintTrx, authAc) = mkMintAuthTrx coopDeployment self [authWalletGeorge, authWalletPeter] 10 aaOuts
                      _ <- submitTrx @Void authMintTrx
                      _ <- waitNSlots slotsToWait
                      return authAc
                  )

              authQAtGeorge <-
                withSuccessContract @String
                  3
                  ( \_ -> do
                      logInfo @String "Running as authWalletGeorge"
                      self <- ownFirstPaymentPubKeyHash
                      authOuts <- findOutsAtHolding' self authAc
                      _ <- burnAuths coopDeployment self authOuts
                      _ <- waitNSlots (slotsToWait * 2)
                      authOuts' <- findOutsAtHolding' self authAc
                      return $ [ciValueOf authAc out | out <- toList authOuts']
                  )

              authQAtPeter <-
                withSuccessContract @String
                  4
                  ( \_ -> do
                      logInfo @String "Running as authWalletPeter"
                      self <- ownFirstPaymentPubKeyHash
                      authOuts <- findOutsAtHolding' self authAc
                      _ <- burnAuths coopDeployment self authOuts
                      _ <- waitNSlots (slotsToWait * 2)
                      authOuts' <- findOutsAtHolding' self authAc
                      return $ [ciValueOf authAc out | out <- toList authOuts']
                  )

              withContract @String (const $ return (authQAtGeorge, authQAtPeter))
          )
          [shouldSucceed, shouldYield ([], [])]
    , runAfter "burn-auth" $
        assertExecutionWith
          testOpts
          "mint-combined-cert-auth"
          (initAda [200] <> initAda [200] <> initAda [200] <> initAda [200] <> initAda [200]) -- TODO: Make this more explicit somehow
          ( do
              (coopDeployment, certRedeemerAc) <- godDeploysCoop coopPlutus

              withContractAs @String
                1
                ( \[_, _, authWalletGeorge, authWalletPeter] -> do
                    logInfo @String "Running as aaWallet"
                    _ <- waitNSlots slotsToWait
                    self <- ownFirstPaymentPubKeyHash
                    aaOuts <- findOutsAtHoldingAa self coopDeployment
                    now <- currentTime
                    let validityInterval = interval now (now + 100_000)
                    let (mintAuthTrx, authAc) = mkMintAuthTrx coopDeployment self [authWalletGeorge, authWalletPeter] 10 aaOuts
                        (mintCertTrx, certAc) = mkMintCertTrx coopDeployment self certRedeemerAc validityInterval aaOuts
                    _ <- submitTrx @Void (mintAuthTrx <> mintCertTrx)
                    _ <- waitNSlots slotsToWait
                    georgesOuts <- findOutsAtHolding' authWalletGeorge authAc
                    petersOuts <- findOutsAtHolding' authWalletPeter authAc
                    certVOuts <- findOutsAtCertVWithCERT coopDeployment
                    return $
                      [ciValueOf authAc out | out <- toList georgesOuts]
                        <> [ciValueOf authAc out | out <- toList petersOuts]
                        <> [ciValueOf certAc out | out <- toList certVOuts]
                )
          )
          [shouldSucceed, shouldYield [10, 10, 1]]
    ]

godDeploysCoop :: CoopPlutus -> ReaderT (ClusterEnv, NonEmpty BpiWallet) IO (CoopDeployment, AssetClass)
godDeploysCoop coopPlutus =
  withSuccessContract @String
    0
    ( \(aaWallet : certRedeemerWallet : _) -> do
        logInfo @String "Running as godWallet"
        self <- ownFirstPaymentPubKeyHash
        _ <- makeCollateralOuts self 5 20_000_000
        _ <- waitNSlots slotsToWait
        coopDeployment <- deployCoop coopPlutus self aaWallet 3
        _ <- waitNSlots slotsToWait
        (_, (certRedeemerAc, _)) <- mintCertRedeemers coopPlutus self certRedeemerWallet 100
        _ <- waitNSlots slotsToWait
        return (coopDeployment, certRedeemerAc)
    )
