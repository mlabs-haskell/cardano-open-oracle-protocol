{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Aux (runAfter, withSuccessContract)
import BotPlutusInterface.Types (LogContext (ContractLog), LogLevel (Debug), LogType (AnyLog, CollateralLog))
import Cardano.Proto.Aux (fromCardano)
import Control.Lens ((.~))
import Control.Monad.Reader (ReaderT)
import Coop.Pab (burnAuths, burnCerts, deployCoop, findOutsAtCertVWithCERT, findOutsAtHoldingAa, mintAuthAndCert, mintCertRedeemers, mkMintAuthTrx, mkMintCertTrx, runMintFsTx, runRedistributeAuthsTrx)
import Coop.Pab.Aux (DeployMode (DEPLOY_DEBUG), ciValueOf, datumFromTxOut, deplFsCs, deplFsVAddress, findOutsAt', findOutsAtHolding, findOutsAtHolding', findOutsAtHoldingCurrency, interval', loadCoopPlutus, mkMintOneShotTrx, submitTrx)
import Coop.Types (AuthDeployment (ad'authorityAc, ad'certV), CoopDeployment (cd'auth, cd'coopAc), CoopPlutus (cp'mkOneShotMp), FsDatum)
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.Foldable (Foldable (toList))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Void (Void)
import GHC.Natural (Natural)
import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash), interval)
import Ledger.Ada (adaValueOf)
import Ledger.Value (AssetClass)
import Plutus.Contract (currentNodeClientTimeRange, logInfo, ownFirstPaymentPubKeyHash, throwError, waitNSlots)
import Plutus.Script.Utils.V2.Address (mkValidatorAddress)
import Plutus.V2.Ledger.Api (Extended (Finite, NegInf, PosInf), POSIXTime, fromData, toBuiltin, toData)
import Plutus.V2.Ledger.Api qualified as AssocMap
import Proto.TxBuilderService_Fields (factStatements, fs, fsId, gcAfter, submitter)
import Test.Plutip.Contract (assertExecutionWith, initAda, withCollateral, withContract, withContractAs)
import Test.Plutip.Internal.Types (ClusterEnv)
import Test.Plutip.LocalCluster (BpiWallet, withConfiguredCluster)
import Test.Plutip.Options (TraceOption (ShowBudgets, ShowTrace, ShowTraceButOnlyContext))
import Test.Plutip.Predicate (shouldSucceed, shouldYield)
import Test.Tasty (TestTree, defaultMain)

main :: IO ()
main = do
  coopPlutus <- loadCoopPlutus DEPLOY_DEBUG
  defaultMain (tests coopPlutus)

slotsToWait :: Natural
slotsToWait = 100

testOpts :: [TraceOption]
testOpts = [ShowTraceButOnlyContext ContractLog (Debug [AnyLog, CollateralLog]), ShowBudgets, ShowTrace]

tests :: CoopPlutus -> TestTree
tests coopPlutus =
  withConfiguredCluster
    def
    "coop-pab-tests"
    [ assertExecutionWith
        testOpts
        "mint-one-shot"
        (withCollateral $ initAda [100] <> initAda [100])
        ( withContract @String
            ( \[oneShotWallet] -> do
                _ <- waitNSlots 10
                self <- ownFirstPaymentPubKeyHash
                outs <- findOutsAt' @Void self (\_ _ -> True)
                let (trx, oneShotAc) = mkMintOneShotTrx self oneShotWallet (head . Map.toList $ outs) (cp'mkOneShotMp coopPlutus) 1
                submitTrx @Void trx
                found <- findOutsAtHolding' oneShotWallet oneShotAc
                return $ length found
            )
        )
        [shouldSucceed, shouldYield 1]
    , runAfter "mint-one-shot" $
        assertExecutionWith
          testOpts
          "deploy-coop"
          -- god <> aa
          (initAda [200, 200, 200] <> initAda [200])
          ( withContract @String
              ( \[aaWallet] -> do
                  logInfo @String "Running as godWallet"
                  self <- ownFirstPaymentPubKeyHash
                  coopDeployment <- deployCoop coopPlutus aaWallet 3 6
                  let aaAc = ad'authorityAc . cd'auth $ coopDeployment
                      coopAc = cd'coopAc coopDeployment
                  aaOuts <- findOutsAtHolding' aaWallet aaAc
                  coopOuts <- findOutsAtHolding' self coopAc
                  return $
                    [ciValueOf aaAc out | out <- toList aaOuts]
                      <> [ciValueOf coopAc out | out <- toList coopOuts]
              )
          )
          [shouldSucceed, shouldYield [6, 1]]
    , runAfter "deploy-coop" $
        assertExecutionWith
          testOpts
          "mint-cert"
          -- WALLETS: god <> aa <> certRdmr
          (withCollateral $ initAda [50, 50, 50] <> initAda [200] <> initAda [200])
          ( do
              (coopDeployment, certRdmrAc) <- genesis coopPlutus

              withContractAs @String
                1
                ( \_ -> do
                    logInfo @String "Running as aaWallet"
                    self <- ownFirstPaymentPubKeyHash
                    aaOuts <- findOutsAtHoldingAa self coopDeployment
                    (_, now) <- currentNodeClientTimeRange
                    let validityInterval = interval now (now + 100_000)
                        (mintCertTrx, certAc) = mkMintCertTrx coopDeployment self certRdmrAc validityInterval aaOuts
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
          -- WALLETS: god <> aa <> certR
          (withCollateral $ initAda [500, 500, 500] <> initAda [200] <> initAda [200])
          ( do
              (coopDeployment, certRdmrAc) <- genesis coopPlutus

              _ <-
                withSuccessContract @String
                  1
                  ( \[_god, _certR] -> do
                      logInfo @String "Running as aaWallet"
                      self <- ownFirstPaymentPubKeyHash
                      aaOuts <- findOutsAtHoldingAa self coopDeployment
                      (_, now) <- currentNodeClientTimeRange
                      let certValidUntil = now + 5
                          validityInterval = interval now certValidUntil
                          (mintCertTrx, _) = mkMintCertTrx coopDeployment self certRdmrAc validityInterval aaOuts
                      submitTrx @Void mintCertTrx
                  )

              withContractAs @String
                2
                ( \[_god, _aa] -> do
                    logInfo @String "Running as certRdmrWallet"
                    _ <- waitNSlots 5 -- NOTE: Should be enough for the $CERT to invalidate
                    burnCerts coopDeployment certRdmrAc
                )
          )
          [shouldSucceed]
    , runAfter "burn-cert" $
        assertExecutionWith
          testOpts
          "mint-auth"
          -- WALLETS: god <> aa <> certRdmr <> authGeorge <> authPeter
          (withCollateral $ initAda [50, 50, 50] <> initAda [200] <> initAda [200] <> initAda [200] <> initAda [200])
          ( do
              (coopDeployment, _) <- genesis coopPlutus

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
          -- WALLETS: god <> aa <> certRdmr <> authGeorge <> authPeter
          (withCollateral $ initAda [50, 50, 50] <> initAda [200] <> initAda [200] <> initAda [200] <> initAda [200])
          ( do
              (coopDeployment, _) <- genesis coopPlutus

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
          -- WALLETS: god <> aa <> certRdmr <> authGeorge <> authPeter
          (withCollateral $ initAda [50, 50, 50] <> initAda [200] <> initAda [200] <> initAda [200] <> initAda [200])
          ( do
              (coopDeployment, certRdmrAc) <- genesis coopPlutus

              withContractAs @String
                1
                ( \[_, _, authWalletGeorge, authWalletPeter] -> do
                    logInfo @String "Running as aaWallet"
                    (_, now) <- currentNodeClientTimeRange
                    (certAc, authAc) <- mintAuthAndCert coopDeployment [authWalletGeorge, authWalletPeter] 10 certRdmrAc now (now + 100_000)
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
        assertExecutionWith -- FIXME: This test shouldSucceed
          testOpts
          "mint-fs"
          -- WALLETS: god <> aa <> certRdmr <> authWallet <> submitterWallet <> feeWallet
          (withCollateral $ initAda [50, 50, 50] <> initAda [200] <> initAda [200] <> initAda [200] <> initAda [200] <> initAda [200])
          ( do
              (coopDeployment, certRdmrAc) <- genesis coopPlutus
              _ <-
                withSuccessContract @String
                  1
                  ( \[_god, _certR, authWallet, _sub, _fee] -> do
                      self <- ownFirstPaymentPubKeyHash
                      logInfo @String $ "Running as aaWallet " <> show self
                      _ <- waitNSlots slotsToWait
                      aaOuts <- findOutsAtHoldingAa self coopDeployment
                      (_, now) <- currentNodeClientTimeRange
                      let validityInterval = interval' (Finite now) PosInf
                      let (mintAuthTrx, authAc) = mkMintAuthTrx coopDeployment self [authWallet] 5 aaOuts
                          (mintCertTrx, certAc) = mkMintCertTrx coopDeployment self certRdmrAc validityInterval aaOuts
                      submitTrx @Void (mintAuthTrx <> mintCertTrx)
                      return (authAc, certAc)
                  )

              _ <-
                withSuccessContract @String
                  3
                  ( \[_god, _aa, _certR, _sub, _fee] -> do
                      self <- ownFirstPaymentPubKeyHash
                      logInfo @String $ "Running as authWallet " <> show self
                      runRedistributeAuthsTrx coopDeployment self 5
                  )

              withContractAs @String
                4
                ( \[_god, _aa, _certR, authWallet, feeWallet] -> do
                    self <- ownFirstPaymentPubKeyHash
                    logInfo @String $ "Running as submitterWallet " <> show self
                    pSubmitter <- fromCardano (unPaymentPubKeyHash self)
                    pNegInf <- fromCardano (NegInf :: Extended POSIXTime)
                    bytesPrData <- maybe (throwError "Must bytes") return $ fromData . toData $ toBuiltin @ByteString "some bytes"
                    intPrData <- maybe (throwError "Must int") return $ fromData . toData @Integer $ 1337
                    listPrData <- maybe (throwError "Must list") return $ fromData . toData @[Integer] $ [1, 2, 3]
                    mapPrData <- maybe (throwError "Must map") return $ fromData . toData @(AssocMap.Map Integer Integer) $ AssocMap.fromList [(1, 1), (2, 2)]
                    constrPrData <- maybe (throwError "Must constr") return $ fromData . toData @(Maybe Integer) $ Just 12

                    let fsInfos =
                          [ defMessage
                              & fsId .~ "fsIdA"
                              & fs .~ bytesPrData
                              & gcAfter .~ pNegInf
                          , defMessage
                              & fsId .~ "fsIdB"
                              & fs .~ intPrData
                              & gcAfter .~ pNegInf
                          , defMessage
                              & fsId .~ "fsIdC"
                              & fs .~ listPrData
                              & gcAfter .~ pNegInf
                          , defMessage
                              & fsId .~ "fsIdD"
                              & fs .~ mapPrData
                              & gcAfter .~ pNegInf
                          , defMessage
                              & fsId .~ "fsIdE"
                              & fs .~ constrPrData
                              & gcAfter .~ pNegInf
                          ]
                        req =
                          defMessage
                            & factStatements .~ fsInfos
                            & submitter .~ pSubmitter
                    logInfo @String (show fsInfos)
                    _ <- runMintFsTx coopDeployment [authWallet] (adaValueOf 13, feeWallet) req

                    outs <- findOutsAtHoldingCurrency (deplFsVAddress coopDeployment) (deplFsCs coopDeployment)
                    feeOut <- findOutsAt' @Void feeWallet (\v _ -> v == adaValueOf 13)
                    return (length feeOut, length [datumFromTxOut @FsDatum out | out <- toList outs])
                )
          )
          [shouldSucceed, shouldYield (1, 5)]
    ]

genesis :: CoopPlutus -> ReaderT (ClusterEnv, NonEmpty BpiWallet) IO (CoopDeployment, AssetClass)
genesis coopPlutus = do
  certRdmrAc <-
    withSuccessContract @String
      2
      ( \(_godWallet : _aaWallet : _) -> do
          logInfo @String "Running as certRdmWallet"
          mintCertRedeemers coopPlutus 100
      )

  withSuccessContract @String
    0
    ( \(aaWallet : _certRdmrWallet : _) -> do
        logInfo @String "Running as godWallet"
        coopDeployment <- deployCoop coopPlutus aaWallet 3 6
        return (coopDeployment, certRdmrAc)
    )
