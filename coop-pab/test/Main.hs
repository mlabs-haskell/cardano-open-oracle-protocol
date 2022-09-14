{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Aux (runAfter, withSuccessContract)
import BotPlutusInterface.Types (LogContext (ContractLog), LogLevel (Debug), LogType (AnyLog, CollateralLog))
import Control.Monad.Reader (ReaderT)
import Coop.Pab (burnAuths, burnCerts, deployCoop, findOutsAtCertVWithCERT, findOutsAtHoldingAa, mintCertRedeemers, mkMintAuthTrx, mkMintCertTrx, mkMintFsTrx)
import Coop.Pab.Aux (DeployMode (DEPLOY_DEBUG), ciValueOf, datumFromTxOut, findOutsAt', findOutsAtHolding, findOutsAtHolding', interval', loadCoopPlutus, mkMintNftTrx, submitTrx)
import Coop.Types (AuthDeployment (ad'authorityAc, ad'certV), CertDatum (cert'validity), CoopDeployment (cd'auth, cd'coopAc, cd'fsV), CoopPlutus (cp'mkNftMp), FsDatum (FsDatum))
import Data.Bool (bool)
import Data.Default (def)
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import GHC.Natural (Natural)
import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash), interval)
import Ledger.Value (AssetClass)
import Plutus.Contract (currentTime, logInfo, ownFirstPaymentPubKeyHash, throwError, waitNSlots)
import Plutus.Script.Utils.V2.Address (mkValidatorAddress)
import Plutus.V2.Ledger.Api (Extended (Finite, NegInf, PosInf), Interval (ivTo), UpperBound (UpperBound))
import Test.Plutip.Contract (assertExecutionWith, initAda, withCollateral, withContract, withContractAs)
import Test.Plutip.Internal.Types (ClusterEnv)
import Test.Plutip.LocalCluster (BpiWallet, withConfiguredCluster)
import Test.Plutip.Options (TraceOption (ShowBudgets, ShowTraceButOnlyContext))
import Test.Plutip.Predicate (shouldSucceed, shouldYield)
import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Text.Printf (printf)

main :: IO ()
main = do
  coopPlutus <- loadCoopPlutus DEPLOY_DEBUG
  defaultMain (tests coopPlutus)

slotsToWait :: Natural
slotsToWait = 100

testOpts :: [TraceOption]
testOpts = [ShowTraceButOnlyContext ContractLog (Debug [AnyLog, CollateralLog]), ShowBudgets]

tests :: CoopPlutus -> TestTree
tests coopPlutus =
  withConfiguredCluster
    def
    "coop-pab-tests"
    [ assertExecutionWith
        testOpts
        "mint-nft"
        (withCollateral $ initAda [100] <> initAda [100])
        ( withContract @String
            ( \[nftWallet] -> do
                self <- ownFirstPaymentPubKeyHash
                outs <- findOutsAt' @Void self (\_ _ -> True)
                let (trx, nftAc) = mkMintNftTrx self nftWallet (head . Map.toList $ outs) (cp'mkNftMp coopPlutus) 1
                submitTrx @Void trx
                found <- findOutsAtHolding' nftWallet nftAc
                return $ length found
            )
        )
        [shouldSucceed, shouldYield 1]
    , runAfter "mint-nft" $
        assertExecutionWith
          testOpts
          "deploy-coop"
          -- god <> aa
          (withCollateral $ initAda [200] <> initAda [200])
          ( withContract @String
              ( \[aaWallet] -> do
                  logInfo @String "Running as godWallet"
                  self <- ownFirstPaymentPubKeyHash
                  coopDeployment <- deployCoop coopPlutus self aaWallet 3
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
          -- god <> aa <> certR
          (withCollateral $ initAda [50, 50, 50] <> initAda [200] <> initAda [200])
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
                    submitTrx @Void mintCertTrx
                    certOuts <- findOutsAtHolding (mkValidatorAddress . ad'certV . cd'auth $ coopDeployment) certAc
                    return [ciValueOf certAc out | out <- toList certOuts]
                )
          )
          [shouldSucceed, shouldYield [1]]
    , runAfter "mint-cert" $
        assertExecutionWith
          testOpts
          "burn-cert"
          -- god <> aa <> certR
          (withCollateral $ initAda [50, 50, 50] <> initAda [200] <> initAda [200])
          ( do
              (coopDeployment, certRedeemerAc) <- godDeploysCoop coopPlutus

              certAc <-
                withSuccessContract @String
                  1
                  ( \[_god, _certR] -> do
                      logInfo @String "Running as aaWallet"
                      self <- ownFirstPaymentPubKeyHash
                      aaOuts <- findOutsAtHoldingAa self coopDeployment
                      now <- currentTime
                      let certValidUntil = now + 5
                          validityInterval = interval now certValidUntil
                          (mintCertTrx, certAc) = mkMintCertTrx coopDeployment self certRedeemerAc validityInterval aaOuts
                      submitTrx @Void mintCertTrx
                      return certAc
                  )

              withContractAs @String
                2
                ( \[_god, _aa] -> do
                    logInfo @String "Running as certRedeemerWallet"
                    _ <- waitNSlots 5 -- NOTE: Should be enough for the $CERT to invalidate
                    self <- ownFirstPaymentPubKeyHash
                    certRedeemerOuts <- findOutsAtHolding' self certRedeemerAc
                    certOuts <- findOutsAtHolding (mkValidatorAddress . ad'certV . cd'auth $ coopDeployment) certAc
                    logInfo @String $ printf "Found %d $CERT ouputs at @CertV" (length certOuts)
                    bool
                      (throwError "There should be some $CERT inputs")
                      (pure ())
                      $ not (null certOuts)
                    burnCerts coopDeployment self certOuts certRedeemerOuts
                )
          )
          [shouldSucceed]
    , runAfter "burn-cert" $
        assertExecutionWith
          testOpts
          "mint-auth"
          -- god <> aa <> certR <> authGeorge <> authPeter
          (withCollateral $ initAda [50, 50, 50] <> initAda [200] <> initAda [200] <> initAda [200] <> initAda [200])
          ( do
              (coopDeployment, _) <- godDeploysCoop coopPlutus

              withContractAs @String
                1
                ( \[_, _, authWalletGeorge, authWalletPeter] -> do
                    logInfo @String "Running as aaWallet"
                    self <- ownFirstPaymentPubKeyHash
                    aaOuts <- findOutsAtHoldingAa self coopDeployment
                    let (authMintTrx, authAc) = mkMintAuthTrx coopDeployment self [authWalletGeorge, authWalletPeter] 10 aaOuts
                    submitTrx @Void authMintTrx
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
          -- god <> aa <> certR <> authGeorge <> authPeter
          (withCollateral $ initAda [50, 50, 50] <> initAda [200] <> initAda [200] <> initAda [200] <> initAda [200])
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
          -- god <> aa <> certR <> authGeorge <> authPeter
          (withCollateral $ initAda [50, 50, 50] <> initAda [200] <> initAda [200] <> initAda [200] <> initAda [200])
          ( do
              (coopDeployment, certRedeemerAc) <- godDeploysCoop coopPlutus

              withContractAs @String
                1
                ( \[_, _, authWalletGeorge, authWalletPeter] -> do
                    logInfo @String "Running as aaWallet"
                    self <- ownFirstPaymentPubKeyHash
                    aaOuts <- findOutsAtHoldingAa self coopDeployment
                    now <- currentTime
                    let validityInterval = interval now (now + 100_000)
                    let (mintAuthTrx, authAc) = mkMintAuthTrx coopDeployment self [authWalletGeorge, authWalletPeter] 10 aaOuts
                        (mintCertTrx, certAc) = mkMintCertTrx coopDeployment self certRedeemerAc validityInterval aaOuts
                    submitTrx @Void (mintAuthTrx <> mintCertTrx)
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
    , runAfter "mint-combined-cert-auth" $
        (expectFailBecause "Still no reference inputs support" .)
          <$> assertExecutionWith
            testOpts
            "mint-fs"
            -- god <> aa <> certRedeemer <> authWallet <> submitterWallet
            (withCollateral $ initAda [50, 50, 50] <> initAda [200] <> initAda [200] <> initAda [200] <> initAda [200])
            ( do
                (coopDeployment, certRedeemerAc) <- godDeploysCoop coopPlutus

                (authAc, certAc) <-
                  withSuccessContract @String
                    1
                    ( \[_god, _certR, authWallet, _sub] -> do
                        logInfo @String "Running as aaWallet"
                        _ <- waitNSlots slotsToWait
                        self <- ownFirstPaymentPubKeyHash
                        aaOuts <- findOutsAtHoldingAa self coopDeployment
                        now <- currentTime
                        let validityInterval = interval' (Finite now) PosInf
                        let (mintAuthTrx, authAc) = mkMintAuthTrx coopDeployment self [authWallet] 1 aaOuts -- TODO: Enable $AUTH outputs with more than 1 Q
                            (mintCertTrx, certAc) = mkMintCertTrx coopDeployment self certRedeemerAc validityInterval aaOuts
                        submitTrx @Void (mintAuthTrx <> mintCertTrx)
                        return (authAc, certAc)
                    )

                withContractAs @String
                  3
                  ( \[_god, _aa, _certR, submitterWallet] -> do
                      logInfo @String "Running as authWallet"
                      self <- ownFirstPaymentPubKeyHash
                      authOuts <- findOutsAtHolding' self authAc
                      authOut <- case Map.toList authOuts of
                        [] -> throwError "Must find at least one $AUTH token"
                        (out : _) -> return out

                      certOuts <- findOutsAtHolding (mkValidatorAddress . ad'certV . cd'auth $ coopDeployment) certAc
                      certOut <- case Map.toList certOuts of
                        [] -> throwError "Must find at least one $CERT token"
                        (out : _) -> return out
                      mayCertDatum <- datumFromTxOut $ snd certOut
                      certDatum <-
                        maybe
                          (throwError "Must find a CertDatum")
                          pure
                          mayCertDatum
                      now <- currentTime
                      let (UpperBound toExt _) = ivTo . cert'validity $ certDatum
                      let fsDatum = FsDatum "aa" "aa" NegInf (unPaymentPubKeyHash submitterWallet)
                          (mintFsTrx, fsAc) =
                            mkMintFsTrx
                              coopDeployment
                              self
                              (interval' (Finite now) toExt)
                              fsDatum
                              authOut
                              (certOut, certDatum)
                              submitterWallet
                      -- FIXME: This test passes allthough it shouldn't
                      submitTrx @Void mintFsTrx
                      fsOuts <- findOutsAtHolding (mkValidatorAddress . cd'fsV $ coopDeployment) fsAc
                      return [ciValueOf fsAc out | out <- toList fsOuts]
                  )
            )
            [shouldSucceed, shouldYield [1]]
    ]

godDeploysCoop :: CoopPlutus -> ReaderT (ClusterEnv, NonEmpty BpiWallet) IO (CoopDeployment, AssetClass)
godDeploysCoop coopPlutus =
  withSuccessContract @String
    0
    ( \(aaWallet : certRedeemerWallet : _) -> do
        logInfo @String "Running as godWallet"
        self <- ownFirstPaymentPubKeyHash
        certRedeemerAc <- mintCertRedeemers coopPlutus self certRedeemerWallet 100
        coopDeployment <- deployCoop coopPlutus self aaWallet 3
        return (coopDeployment, certRedeemerAc)
    )
