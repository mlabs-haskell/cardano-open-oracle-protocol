module Main (main) where

import Cardano.Oracle.Aux (DeployMode (DEPLOY_PROD), loadCoopPlutus)
import Cardano.Oracle.Pab (createInstanceCs, deploy)
import Cardano.Oracle.Types (CoopPlutus (cp'instanceMintingPolicy))
import Data.Default (def)
import Plutus.Contract (waitNSlots)
import Test.Plutip.Contract (assertExecutionWith, initAda, withContract)
import Test.Plutip.LocalCluster (withConfiguredCluster)
import Test.Plutip.Options (TraceOption (ShowBudgets, ShowTrace))
import Test.Plutip.Predicate (shouldFail, shouldSucceed)
import Test.Plutip.Predicate qualified as Predicate
import Test.Tasty (TestTree, defaultMain)

main :: IO ()
main = do
  coopPlutus <- loadCoopPlutus DEPLOY_PROD
  defaultMain (tests coopPlutus)

tests :: CoopPlutus -> TestTree
tests coopPlutus =
  withConfiguredCluster
    def
    "oracle-pab-tests"
    [ assertExecutionWith
        [ShowTrace, ShowBudgets]
        "createInstanceCs"
        (initAda (100 : replicate 10 7))
        ( withContract
            ( const $ do
                _ <- waitNSlots 5
                _ <- createInstanceCs @String (cp'instanceMintingPolicy coopPlutus)
                waitNSlots 10
            )
        )
        [shouldSucceed, Predicate.not shouldFail]
    , assertExecutionWith
        [ShowTrace, ShowBudgets]
        "deploy"
        (initAda (100 : replicate 10 7))
        ( withContract
            ( const $ do
                _ <- waitNSlots 5
                _ <- deploy @String coopPlutus
                waitNSlots 10
            )
        )
        [shouldSucceed, Predicate.not shouldFail]
    ]
