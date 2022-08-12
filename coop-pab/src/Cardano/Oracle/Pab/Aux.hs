module Cardano.Oracle.Pab.Aux (loadCoopPlutus, runBpi, DeployMode (..), minUtxoAdaValue) where

import BotPlutusInterface.Contract (runContract)
import BotPlutusInterface.Types (ContractEnvironment (ContractEnvironment), ContractState (ContractState), PABConfig, ceContractInstanceId, ceContractLogs, ceContractState, ceContractStats, cePABConfig)
import Cardano.Oracle.Types (CoopPlutus)
import Control.Concurrent.STM (newTVarIO)
import Data.Aeson (ToJSON, decodeFileStrict)
import Data.UUID.V4 qualified as UUID
import Ledger.Ada (lovelaceValueOf)
import Plutus.Contract (Contract, ContractInstanceId)
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import Plutus.V1.Ledger.Api (Value)
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import System.Process (callProcess)
import Wallet.Types (ContractInstanceId (ContractInstanceId))

data DeployMode = DEPLOY_PROD | DEPLOY_DEBUG deriving stock (Show, Read, Eq)

loadCoopPlutus :: DeployMode -> IO CoopPlutus
loadCoopPlutus mode = do
  tempDir <- getTemporaryDirectory
  let compileMode = if mode == DEPLOY_PROD then "COMPILE_PROD" else "COMPILE_DEBUG"
      coopPlutusFp = tempDir </> "coop-plutus.json"
  callProcess "coop-plutus-cli" ["compile", "--mode", compileMode, "--file", coopPlutusFp]
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

minUtxoAdaValue :: Value
minUtxoAdaValue = lovelaceValueOf 2_000_000

-- data PLog e a = PLog
--   { pl'processName :: Text
--   , pl'event :: PEvent e a
--   }
-- data PEvent e a
--   = PStart
--   | PEnd (Either e a)
--   | PEmit Text PEmitInfo

-- data PEmitInfo = forall i. (ToJSON i, Generic i) => PEmitInfo i deriving (Generic) via (i) deriving anyclass (ToJSON)

-- instance (ToJSON e, ToJSON a) => ToJSON (PLog e a)

-- plogged :: (ToJSON e, ToJSON a, ToJSON event) => Text -> (Contract w s Text () -> Contract w s Text a) -> Contract w s Text a
-- plogged processName contractWithLogger = do
--   logInfo $ PStart @(PLog e a event) processName
--   --contractWithLogger (logInfo @String )
--   return undefined
