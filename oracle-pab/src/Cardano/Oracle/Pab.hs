module Cardano.Oracle.Pab (
  createInstanceCs,
  deploy,
) where

import Cardano.Oracle.Types (CoopDeployment (CoopDeployment), CoopPlutus (cp'instanceMintingPolicy, cp'resourceMintingPolicy, cp'resourceValidator), ResourceMintingParams (ResourceMintingParams), ResourceValidatorParams (ResourceValidatorParams))
import Data.Map (keys)
import Data.Text (Text)
import Data.Void (Void)
import Ledger (applyArguments, getCardanoTxId, scriptCurrencySymbol)
import Ledger.Address (pubKeyHashAddress)
import Plutus.Contract (Contract, ownFirstPaymentPubKeyHash, submitTxConstraintsWith, throwError, utxosAt)
import Plutus.Contract.Constraints (mintingPolicy, mustMintValue, mustPayToPubKey, mustSpendPubKeyOutput, unspentOutputs)
import Plutus.Contract.Logging (logInfo)
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import Plutus.V1.Ledger.Api (
  CurrencySymbol,
  MintingPolicy (MintingPolicy),
  Script,
  TokenName (TokenName),
  TxId (getTxId),
  TxOutRef (txOutRefId),
  Validator (Validator),
  toData,
 )
import Plutus.V1.Ledger.Value qualified as Value
import Test.Plutip.Internal.BotPlutusInterface.Setup ()
import Test.Plutip.Internal.LocalCluster ()
import Text.Printf (printf)

createInstanceCs :: Script -> Contract w s Text (TxId, CurrencySymbol)
createInstanceCs instMp' = do
  let logI m = logInfo @String ("createInstaceCs: " <> m)
  pkh <- ownFirstPaymentPubKeyHash
  utxos <- utxosAt (pubKeyHashAddress pkh Nothing)
  case keys utxos of
    [] -> do
      throwError "no utxo found"
    oref : _ -> do
      logI $ "Using oref " <> show oref
      let instTokenName = TokenName . getTxId . txOutRefId $ oref
          instMp = MintingPolicy $ applyArguments instMp' [toData instTokenName, toData oref]
          instCs = scriptCurrencySymbol instMp
          val = Value.singleton instCs instTokenName 1
          lookups =
            mconcat
              [ mintingPolicy instMp
              , unspentOutputs utxos
              ]
          tx =
            mconcat
              [ mustMintValue val
              , mustSpendPubKeyOutput oref
              , mustPayToPubKey pkh val
              ]
      logI $ show (toData instTokenName)
      logI $ show (toData oref)
      tx <- submitTxConstraintsWith @Void lookups tx
      logI $ printf "forged %s" (show val)
      logI "Finished"
      return (getCardanoTxId tx, instCs)

deploy :: CoopPlutus -> Contract w s Text CoopDeployment
deploy coopPlutus = do
  (_, cs) <- createInstanceCs (cp'instanceMintingPolicy coopPlutus)
  let rvp = ResourceValidatorParams cs
      resV = Validator $ applyArguments (cp'resourceValidator coopPlutus) [toData rvp]
      rmp = ResourceMintingParams cs (mkValidatorAddress resV)
      resMp = MintingPolicy $ applyArguments (cp'resourceMintingPolicy coopPlutus) [toData rmp]
  return $ CoopDeployment rmp resMp rvp resV
