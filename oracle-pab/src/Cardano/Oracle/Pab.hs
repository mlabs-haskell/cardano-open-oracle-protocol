module Cardano.Oracle.Pab (
  createInstanceCs,
) where

import Cardano.Oracle.Types (CoopPlutus (cp'instanceMintingPolicy))
import Data.Map (keys)
import Data.Text (Text)
import Data.Void (Void)
import Ledger (applyArguments, getCardanoTxId, scriptCurrencySymbol)
import Ledger.Address (pubKeyHashAddress)
import Plutus.Contract (Contract, ownFirstPaymentPubKeyHash, submitTxConstraintsWith, throwError, utxosAt)
import Plutus.Contract.Constraints (mintingPolicy, mustMintValue, mustPayToPubKey, mustSpendPubKeyOutput, unspentOutputs)
import Plutus.Contract.Logging (logInfo)
import Plutus.V1.Ledger.Api (
  CurrencySymbol,
  MintingPolicy (MintingPolicy),
  TokenName (TokenName),
  TxId (getTxId),
  TxOutRef (txOutRefId),
  toData,
 )
import Plutus.V1.Ledger.Value qualified as Value
import Test.Plutip.Internal.BotPlutusInterface.Setup ()
import Test.Plutip.Internal.LocalCluster ()
import Text.Printf (printf)

createInstanceCs :: CoopPlutus -> Contract w s Text (Maybe (TxId, CurrencySymbol))
createInstanceCs coopPlutus = do
  let logI m = logInfo @String ("createInstaceCs: " <> m)
      instMp' = cp'instanceMintingPolicy coopPlutus
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
      return $ Just (getCardanoTxId tx, instCs)
