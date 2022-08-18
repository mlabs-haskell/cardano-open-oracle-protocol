module Coop.Pab (
  createCoopInstance,
  deploy,
  mintFs,
) where

import Coop.Pab.Aux (minUtxoAdaValue)
import Coop.Types (
  CoopDeployment (CoopDeployment, cd'fsMp, cd'fsV),
  CoopPlutus (cp'mkCoopInstanceMp, cp'mkFsMp, cp'mkFsV),
  FsDatum (FsDatum),
  FsMpParams (FsMpParams),
  FsVParams (FsVParams),
 )
import Data.Map (keys)
import Data.Text (Text)
import Data.Void (Void)
import Ledger (applyArguments, getCardanoTxId, scriptCurrencySymbol, validatorHash)
import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash), pubKeyHashAddress)
import Plutus.Contract (Contract, currentTime, ownFirstPaymentPubKeyHash, submitTxConstraintsWith, throwError, utxosAt)
import Plutus.Contract.Constraints (mintingPolicy, mustBeSignedBy, mustMintValue, mustPayToOtherScript, mustPayToPubKey, mustSpendPubKeyOutput, otherData, otherScript, unspentOutputs)
import Plutus.Contract.Logging (logInfo)
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import Plutus.V1.Ledger.Api (
  CurrencySymbol,
  Datum (Datum),
  MintingPolicy (MintingPolicy),
  PubKeyHash (getPubKeyHash),
  Script,
  ToData (toBuiltinData),
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

createCoopInstance :: Script -> Contract w s Text (TxId, CurrencySymbol)
createCoopInstance mkCoopInstanceMp = do
  let logI m = logInfo @String ("createCoopInstance: " <> m)
  logI "Starting"
  pkh <- ownFirstPaymentPubKeyHash
  utxos <- utxosAt (pubKeyHashAddress pkh Nothing)
  case keys utxos of
    [] -> do
      throwError "no utxo found"
    oref : _ -> do
      logI $ "Using oref " <> show oref
      let coopInstTn = TokenName . getTxId . txOutRefId $ oref
          coopInstMp = MintingPolicy $ applyArguments mkCoopInstanceMp [toData coopInstTn, toData oref]
          coopInstCs = scriptCurrencySymbol coopInstMp
          val = Value.singleton coopInstCs coopInstTn 1
          lookups =
            mconcat
              [ mintingPolicy coopInstMp
              , unspentOutputs utxos
              ]
          tx =
            mconcat
              [ mustMintValue val
              , mustSpendPubKeyOutput oref
              , mustPayToPubKey pkh val
              ]
      tx <- submitTxConstraintsWith @Void lookups tx
      logI $ printf "forged %s" (show val)
      logI "Finished"
      return (getCardanoTxId tx, coopInstCs)

deploy :: CoopPlutus -> Contract w s Text CoopDeployment
deploy coopPlutus = do
  let logI m = logInfo @String ("deploy: " <> m)
  logI "Starting"
  (_, cs) <- createCoopInstance (cp'mkCoopInstanceMp coopPlutus)
  let fsVP = FsVParams cs
      fsV = Validator $ applyArguments (cp'mkFsV coopPlutus) [toData fsVP]
      fsMpP = FsMpParams cs (mkValidatorAddress fsV)
      fsMp = MintingPolicy $ applyArguments (cp'mkFsMp coopPlutus) [toData fsMpP]
  logI "Finished"
  return $ CoopDeployment fsMpP fsMp fsVP fsV

mintFs :: PubKeyHash -> PubKeyHash -> CoopDeployment -> Contract w s Text TxId
mintFs submitterPkh publisherPkh coopDeployment = do
  let logI m = logInfo @String ("mintFs: " <> m)
  logI "Starting"
  let fsMp = cd'fsMp coopDeployment
      fsV = cd'fsV coopDeployment
      fsVAddr = validatorHash fsV
      fsTn = TokenName . getPubKeyHash $ publisherPkh
      fsCs = scriptCurrencySymbol fsMp
      fsVal = Value.singleton fsCs fsTn 1
      fsDatum = Datum . toBuiltinData $ FsDatum submitterPkh publisherPkh "aa" "aa"
      lookups =
        mconcat
          [ mintingPolicy fsMp
          , otherScript fsV
          , otherData fsDatum
          ]
      tx =
        mconcat
          [ mustMintValue fsVal
          , mustBeSignedBy (PaymentPubKeyHash publisherPkh)
          , mustBeSignedBy (PaymentPubKeyHash submitterPkh)
          , mustPayToOtherScript fsVAddr fsDatum (fsVal <> minUtxoAdaValue)
          ]
  tx <- submitTxConstraintsWith @Void lookups tx
  logI "Finished"
  return (getCardanoTxId tx)
