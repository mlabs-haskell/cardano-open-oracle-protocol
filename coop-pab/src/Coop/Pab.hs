module Coop.Pab (
  mkAuthScripts,
  mintCoop,
) where

--  deploy,
--  mintFs,

import Coop.Pab.Aux (minUtxoAdaValue, mintNft)
import Coop.Types (
  AuthMpParams (AuthMpParams),
  CertMpParams (CertMpParams),
  CoopDeployment (CoopDeployment, cd'fsMp, cd'fsV),
  CoopPlutus (cp'mkAuthMp, cp'mkCertMp, cp'mkFsMp, cp'mkFsV, cp'mkNftMp),
  FsDatum (FsDatum),
  FsMpParams (FsMpParams),
  FsVParams (FsVParams),
  cp'certV,
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

-- deploy :: CoopPlutus -> Contract w s Text CoopDeployment
-- deploy coopPlutus = do
--   let logI m = logInfo @String ("deploy: " <> m)
--   logI "Starting"
--   (_, cs) <- mintCoop (cp'mkNftMp coopPlutus)
--   let fsVP = FsVParams cs
--       fsV = Validator $ applyArguments (cp'mkFsV coopPlutus) [toData fsVP]
--       fsMpP = FsMpParams cs (mkValidatorAddress fsV)
--       fsMp = MintingPolicy $ applyArguments (cp'mkFsMp coopPlutus) [toData fsMpP]
--   logI "Finished"
--   return $ CoopDeployment fsMpP fsMp fsVP fsV

mkAuthScripts :: CoopPlutus -> Contract w s Text AuthDeployment
mkAuthScripts coopPlutus = do
  let logI m = logInfo @String ("mkAuthScripts: " <> m)
  logI "Starting"
  -- TODO: Parametrise mkNft by N
  (_, (aaCs, aaTn, _)) <- mintAa (cp'mkNftMp coopPlutus)
  let authMpParams = AuthMpParams (aaCs, aaTn) 1
      certMpParams = CertMpParams (aaCs, aaTn) 1 (mkValidatorAddress certV)
      certV = Validator $ cp'certV coopPlutus
      certMp = MintingPolicy $ applyArguments (cp'mkCertMp coopPlutus) [toData certMpParams]
      authMp = MintingPolicy $ applyArguments (cp'mkAuthMp coopPlutus) [toData authMpParams]
  logI "Finished"
  return $ AuthDeployment (aaCs, aaTn) certV certMp authMp

mintAa :: Script -> Contract w s Text (TxId, (CurrencySymbol, TokenName, Integer))
mintAa mkNftMp = do
  let logI m = logInfo @String ("mintAa: " <> m)
  logI "Starting"
  mintNft mkNftMp 1

mintCoop :: Script -> Contract w s Text (TxId, (CurrencySymbol, TokenName, Integer))
mintCoop mkNftMp = do
  let logI m = logInfo @String ("mintCoop: " <> m)
  logI "Starting"
  mintNft mkNftMp 1

-- mintFs :: PubKeyHash -> PubKeyHash -> CoopDeployment -> Contract w s Text TxId
-- mintFs submitterPkh publisherPkh coopDeployment = do
--   let logI m = logInfo @String ("mintFs: " <> m)
--   logI "Starting"
--   let fsMp = cd'fsMp coopDeployment
--       fsV = cd'fsV coopDeployment
--       fsVAddr = validatorHash fsV
--       fsTn = TokenName . getPubKeyHash $ publisherPkh
--       fsCs = scriptCurrencySymbol fsMp
--       fsVal = Value.singleton fsCs fsTn 1
--       fsDatum = Datum . toBuiltinData $ FsDatum submitterPkh publisherPkh "aa" "aa" 0
--       lookups =
--         mconcat
--           [ mintingPolicy fsMp
--           , otherScript fsV
--           , otherData fsDatum
--           ]
--       tx =
--         mconcat
--           [ mustMintValue fsVal
--           , mustBeSignedBy (PaymentPubKeyHash publisherPkh)
--           , mustBeSignedBy (PaymentPubKeyHash submitterPkh)
--           , mustPayToOtherScript fsVAddr fsDatum (fsVal <> minUtxoAdaValue)
--           ]
--   tx <- submitTxConstraintsWith @Void lookups tx
--   logI "Finished"
--   return (getCardanoTxId tx)
