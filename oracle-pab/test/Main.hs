module Main (main) where

import Cardano.Oracle.Aux (DeployMode (DEPLOY_PROD), loadCoopPlutus)
import Cardano.Oracle.Pab (createInstanceCs)
import Cardano.Oracle.Types (CoopPlutus)
import Data.Default (def)
import Plutus.Contract (throwError, waitNSlots)
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
                mayRes <- createInstanceCs @String coopPlutus
                _ <- maybe (throwError "Failed to createInstanceCs") pure mayRes
                waitNSlots 10
            )
        )
        [shouldSucceed, Predicate.not shouldFail]
    ]
