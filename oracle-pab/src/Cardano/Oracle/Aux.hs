module Cardano.Oracle.Aux (loadCoopPlutus, runBpi, DeployMode (..)) where

import BotPlutusInterface.Contract (runContract)
import BotPlutusInterface.Types (ContractEnvironment (ContractEnvironment), ContractState (ContractState), PABConfig, ceContractInstanceId, ceContractLogs, ceContractState, ceContractStats, cePABConfig)
import Cardano.Oracle.Types (CoopPlutus)
import Control.Concurrent.STM (newTVarIO)
import Data.Aeson (ToJSON, decodeFileStrict)
import Data.UUID.V4 qualified as UUID
import Plutus.Contract (Contract, ContractInstanceId)
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import System.Process (runProcess)
import Wallet.Types (ContractInstanceId (ContractInstanceId))

data DeployMode = DEPLOY_PROD | DEPLOY_DEBUG deriving stock (Show, Read, Eq)

loadCoopPlutus :: DeployMode -> IO CoopPlutus
loadCoopPlutus mode = do
  tempDir <- getTemporaryDirectory
  let compileMode = if mode == DEPLOY_PROD then "COMPILE_PROD" else "COMPILE_DEBUG"
      coopPlutusFp = tempDir </> "coop-plutus.json"
  runProcess "oracle-plutus-cli" ["compile", "--mode", compileMode, "--file", coopPlutusFp] Nothing Nothing Nothing Nothing Nothing
  mayCoopPlutus :: Maybe CoopPlutus <- decodeFileStrict coopPlutusFp
  maybe (fail "Failed decoding CoopPlutus") return mayCoopPlutus

runBpi :: ToJSON w => Monoid w => PABConfig -> Contract w s e a -> IO (ContractInstanceId, Either e a)
runBpi pabConf contract = do
  contractInstanceID <- ContractInstanceId <$> UUID.nextRandom
  contractState <- newTVarIO (ContractState Active mempty)
  contractStats <- newTVarIO mempty
  contractLogs <- newTVarIO mempty

  let contractEnv =
        ContractEnvironment
          { cePABConfig = pabConf
          , ceContractState = contractState
          , ceContractInstanceId = contractInstanceID
          , ceContractStats = contractStats
          , ceContractLogs = contractLogs
          }
  result <- runContract contractEnv contract
  pure (contractInstanceID, result)
