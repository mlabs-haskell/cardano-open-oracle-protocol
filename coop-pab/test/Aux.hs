module Aux (runAfter, withSuccessContract) where

import Control.Monad.Reader (ReaderT)
import Data.Bifunctor (Bifunctor (second))
import Data.List.NonEmpty (NonEmpty)
import Ledger (PaymentPubKeyHash)
import Plutus.Contract (Contract)
import Test.Plutip.Contract (TestWallets, withContractAs)
import Test.Plutip.Contract.Types (TestContractConstraints)
import Test.Plutip.Internal.Types (ClusterEnv, ExecutionResult (outcome))
import Test.Plutip.LocalCluster (BpiWallet)
import Test.Tasty (DependencyType (AllFinish), TestName, TestTree, after)

runAfter ::
  TestName ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
runAfter testName = second (fmap . after AllFinish $ '/' : testName ++ "/")

withSuccessContract :: TestContractConstraints w e a => Int -> ([PaymentPubKeyHash] -> Contract w s e a) -> ReaderT (ClusterEnv, NonEmpty BpiWallet) IO a
withSuccessContract ixWallet contract = do
  res <-
    withContractAs ixWallet contract
  either
    (fail . show)
    (\(res', _) -> pure res')
    $ outcome res
