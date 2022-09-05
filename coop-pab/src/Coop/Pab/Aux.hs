module Coop.Pab.Aux (loadCoopPlutus, runBpi, DeployMode (..), minUtxoAdaValue, mintNft, tokenNameFromTxOutRef) where

import BotPlutusInterface.Contract (runContract)
import BotPlutusInterface.Types (ContractEnvironment (ContractEnvironment), ContractState (ContractState), PABConfig, ceContractInstanceId, ceContractLogs, ceContractState, ceContractStats, cePABConfig)
import Control.Concurrent.STM (newTVarIO)
import Coop.Types (CoopPlutus)
import Crypto.Hash (SHA3_256 (SHA3_256), hashWith)
import Data.Aeson (ToJSON, decodeFileStrict)
import Data.ByteArray (convert)
import Data.ByteString (ByteString, cons)
import Data.Map (keys)
import Data.Text (Text)
import Data.UUID.V4 qualified as UUID
import Data.Void (Void)
import Ledger (applyArguments, getCardanoTxId, pubKeyHashAddress)
import Ledger.Ada (lovelaceValueOf)
import Plutus.Contract (Contract, ContractInstanceId, logInfo, ownFirstPaymentPubKeyHash, submitTxConstraintsWith, throwError, utxosAt)
import Plutus.Contract.Constraints (mintingPolicy, mustMintValue, mustPayToPubKey, mustSpendPubKeyOutput, unspentOutputs)
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import Plutus.Script.Utils.V1.Scripts (scriptCurrencySymbol)
import Plutus.V1.Ledger.Api (CurrencySymbol, MintingPolicy (MintingPolicy), Script, TokenName (TokenName), TxId (getTxId), TxOutRef (txOutRefId, txOutRefIdx), Value, fromBuiltin, toBuiltin, toData)
import Plutus.V1.Ledger.Value qualified as Value
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import System.Process (callProcess)
import Text.Printf (printf)
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

mintNft :: Script -> Integer -> Contract w s Text (TxId, (CurrencySymbol, TokenName, Integer))
mintNft mkNftMp q = do
  let logI m = logInfo @String ("mintNft: " <> m)
  logI "Starting"
  pkh <- ownFirstPaymentPubKeyHash
  utxos <- utxosAt (pubKeyHashAddress pkh Nothing)
  case keys utxos of
    [] -> do
      throwError "no utxo found"
    oref : _ -> do
      logI $ "Using oref " <> show oref
      let nftTn = tokenNameFromTxOutRef oref
          nftMp = MintingPolicy $ applyArguments mkNftMp [toData q, toData nftTn, toData oref]
          nftCs = scriptCurrencySymbol nftMp
          val = Value.singleton nftCs nftTn 1
          lookups =
            mconcat
              [ mintingPolicy nftMp
              , unspentOutputs utxos
              ]
          tx =
            mconcat
              [ mustMintValue val
              , mustSpendPubKeyOutput oref
              , mustPayToPubKey pkh val
              ]
      tx <- submitTxConstraintsWith @Void lookups tx
      logI $ printf "Forged an NFT %s" (show val)
      logI "Finished"
      return (getCardanoTxId tx, (nftCs, nftTn, q))

tokenNameFromTxOutRef :: TxOutRef -> TokenName
tokenNameFromTxOutRef oref =
  let ix = fromInteger . txOutRefIdx $ oref
      txId = fromBuiltin . getTxId . txOutRefId $ oref
      hashedOref = convert @_ @ByteString . hashWith SHA3_256 $ cons ix txId
   in TokenName . toBuiltin $ hashedOref
