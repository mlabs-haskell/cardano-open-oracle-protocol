{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Coop.Pab (mkAuthScripts)
import Coop.Pab.Aux (DeployMode (DEPLOY_DEBUG), loadCoopPlutus)
import Coop.Types (CoopPlutus (cp'mkNftMp))
import Data.Bifunctor (Bifunctor (second))
import Data.Default (def)
import Data.List.NonEmpty (NonEmpty)
import Ledger.Address (PaymentPubKeyHash (unPaymentPubKeyHash))
import Plutus.Contract (logInfo, ownFirstPaymentPubKeyHash, waitNSlots)
import Test.Plutip.Contract (TestWallets, assertExecutionWith, initAda, withContract)
import Test.Plutip.Internal.Types (ClusterEnv)
import Test.Plutip.LocalCluster (BpiWallet, withConfiguredCluster)
import Test.Plutip.Options (TraceOption (ShowBudgets, ShowTrace))
import Test.Plutip.Predicate (shouldFail, shouldSucceed)
import Test.Plutip.Predicate qualified as Predicate
import Test.Tasty (DependencyType (AllFinish), TestName, TestTree, after, defaultMain)

main :: IO ()
main = do
  coopPlutus <- loadCoopPlutus DEPLOY_DEBUG
  defaultMain (tests coopPlutus)

tests :: CoopPlutus -> TestTree
tests coopPlutus =
  withConfiguredCluster
    def
    "coop-pab-tests"
    [ assertExecutionWith
        [ShowTrace, ShowBudgets]
        "create-coop-instance"
        (initAda (100 : replicate 10 7))
        ( withContract
            ( const $ do
                _ <- waitNSlots 5
                _ <- mkAuthScripts @String coopPlutus
                waitNSlots 5
            )
        )
        [shouldSucceed]
        --   assertExecutionWith
        --     [ShowTrace, ShowBudgets]
        --     "create-coop-instance"
        --     (initAda (100 : replicate 10 7))
        --     ( withContract
        --         ( const $ do
        --             _ <- waitNSlots 5
        --             _ <- createCoopInstance @String (cp'mkCoopInstanceMp coopPlutus)
        --             waitNSlots 5
        --         )
        --     )
        --     [shouldSucceed, Predicate.not shouldFail]
        -- , runAfter "create-coop-instance" $
        --     assertExecutionWith
        --       [ShowTrace, ShowBudgets]
        --       "deploy"
        --       (initAda (100 : replicate 10 7))
        --       ( withContract
        --           ( const $ do
        --               _ <- waitNSlots 5
        --               _ <- deploy @String coopPlutus
        --               waitNSlots 5
        --           )
        --       )
        --       [shouldSucceed]
        -- , runAfter "deploy" $
        --     assertExecutionWith
        --       [ShowTrace, ShowBudgets]
        --       "publish-fs"
        --       (initAda (100 : replicate 10 7) <> initAda (100 : replicate 10 7))
        --       ( withContract
        --           ( \(submitterPPkh : _) -> do
        --               _ <- waitNSlots 5
        --               coopDeployment <- deploy @String coopPlutus
        --               _ <- waitNSlots 5
        --               publisherPpkh <- ownFirstPaymentPubKeyHash
        --               logInfo @String ("publish-fs with: submitter = " <> show submitterPPkh <> " publisher = " <> show publisherPpkh)
        --               _ <-
        --                 mintFs
        --                   (unPaymentPubKeyHash submitterPPkh)
        --                   (unPaymentPubKeyHash publisherPpkh)
        --                   coopDeployment
        --               waitNSlots 5
        --           )
        --       )
        --       [shouldSucceed]
    ]

runAfter ::
  TestName ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
runAfter testName = second (fmap . after AllFinish $ '/' : testName ++ "/")
