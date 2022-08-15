module Coop.Pab (
  createCoopInstance,
  deploy,
  mintSof,
) where

import Coop.Pab.Aux (minUtxoAdaValue)
import Coop.Types (
  CoopDeployment (CoopDeployment, cd'sofMp, cd'sofV),
  CoopPlutus (cp'mkCoopInstanceMp, cp'mkSofMp, cp'mkSofV),
  SofDatum (SofDatum),
  SofMpParams (SofMpParams),
  SofVParams (SofVParams),
 )
import Data.Map (keys)
import Data.Text (Text)
import Data.Void (Void)
import Ledger (applyArguments, getCardanoTxId, scriptCurrencySymbol, validatorHash)
import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash), pubKeyHashAddress)
import Plutus.Contract (Contract, ownFirstPaymentPubKeyHash, submitTxConstraintsWith, throwError, utxosAt)
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
  let sofVP = SofVParams cs
      sofV = Validator $ applyArguments (cp'mkSofV coopPlutus) [toData sofVP]
      sofMpP = SofMpParams cs (mkValidatorAddress sofV)
      sofMp = MintingPolicy $ applyArguments (cp'mkSofMp coopPlutus) [toData sofMpP]
  logI "Finished"
  return $ CoopDeployment sofMpP sofMp sofVP sofV

mintSof :: PubKeyHash -> PubKeyHash -> CoopDeployment -> Contract w s Text TxId
mintSof submitterPkh publisherPkh coopDeployment = do
  let logI m = logInfo @String ("mintSof: " <> m)
  logI "Starting"
  let sofMp = cd'sofMp coopDeployment
      sofV = cd'sofV coopDeployment
      sofVAddr = validatorHash sofV
      sofTn = TokenName . getPubKeyHash $ publisherPkh
      sofCs = scriptCurrencySymbol sofMp
      sofVal = Value.singleton sofCs sofTn 1
      sofDatum = Datum . toBuiltinData $ SofDatum submitterPkh publisherPkh "aa" "aa"
      lookups =
        mconcat
          [ mintingPolicy sofMp
          , otherScript sofV
          , otherData sofDatum
          ]
      tx =
        mconcat
          [ mustMintValue sofVal
          , mustBeSignedBy (PaymentPubKeyHash publisherPkh)
          , mustBeSignedBy (PaymentPubKeyHash submitterPkh)
          , mustPayToOtherScript sofVAddr sofDatum (sofVal <> minUtxoAdaValue)
          ]
  tx <- submitTxConstraintsWith @Void lookups tx
  logI "Finished"
  return (getCardanoTxId tx)
