module Cardano.Oracle.Cli.Deploy (DeployOpts (..), DeployMode (..), deploy) where

import BotPlutusInterface.Config (loadPABConfig)
import BotPlutusInterface.Contract (runContract)
import BotPlutusInterface.Types (ContractEnvironment (ContractEnvironment), ContractState (ContractState), PABConfig, ceContractInstanceId, ceContractLogs, ceContractState, ceContractStats, cePABConfig)
import Cardano.Oracle.Pab (createInstanceCs)
import Cardano.Oracle.Types (CoopPlutus)
import Control.Concurrent.STM (newTVarIO)
import Data.Aeson (ToJSON, decodeFileStrict)
import Data.Text (Text)
import Data.UUID.V4 qualified as UUID
import Plutus.Contract (Contract, ContractInstanceId)
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import System.Process (runProcess)
import Wallet.Types (ContractInstanceId (ContractInstanceId))

data DeployMode = DEPLOY_PROD | DEPLOY_DEBUG deriving stock (Show, Read, Eq)

data DeployOpts = DeployOpts
  { do'Mode :: DeployMode
  , do'PabConfig :: FilePath
  , do'DeploymentFile :: FilePath
  }
  deriving stock (Show, Eq)

deploy :: DeployOpts -> IO ()
deploy opts = do
  tempDir <- getTemporaryDirectory
  let compileMode = if do'Mode opts == DEPLOY_PROD then "COMPILE_PROD" else "COMPILE_DEBUG"
  runProcess "oracle-plutus-cli" ["compile", "--mode", compileMode] (Just tempDir) Nothing Nothing Nothing Nothing
  mayCoopPlutus :: Maybe CoopPlutus <- decodeFileStrict (tempDir </> "coop-plutus.json")
  coopPlutus <- maybe (fail "Failed decoding coop-plutus.json") return mayCoopPlutus
  pabConf <-
    either error id <$> loadPABConfig (do'PabConfig opts)
  runBpi @Text pabConf $ createInstanceCs @Text coopPlutus
  return ()

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
