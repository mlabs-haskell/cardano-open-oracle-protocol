module Cardano.Oracle.Pab (
  createInstanceCs,
  deploy,
  mintSof,
) where

import Cardano.Oracle.Types (
  CoopDeployment (CoopDeployment),
  CoopPlutus (cp'instanceMintingPolicy, cp'resourceMintingPolicy, cp'resourceValidator),
  ResourceDatum (ResourceDatum),
  ResourceMintingParams (ResourceMintingParams),
  ResourceValidatorParams (ResourceValidatorParams),
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

createInstanceCs :: Script -> Contract w s Text (TxId, CurrencySymbol)
createInstanceCs instMp' = do
  let logI m = logInfo @String ("createInstaceCs: " <> m)
  logI "Starting"
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

mintSof :: PubKeyHash -> PubKeyHash -> Validator -> MintingPolicy -> Contract w s Text TxId
mintSof submitterPkh publisherPkh sofV sofMp = do
  let logI m = logInfo @String ("mintSof: " <> m)
  logI "Starting"
  let sofTokenName = TokenName . getPubKeyHash $ publisherPkh
      sofCs = scriptCurrencySymbol sofMp
      sofVal = Value.singleton sofCs sofTokenName 1
      sofVAddr = validatorHash sofV
      sofDatum = Datum . toBuiltinData $ ResourceDatum submitterPkh publisherPkh "smtn" "asd"
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
          , mustPayToOtherScript sofVAddr sofDatum sofVal
          ]
  tx <- submitTxConstraintsWith @Void lookups tx
  logI "Finished"
  return (getCardanoTxId tx)

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
