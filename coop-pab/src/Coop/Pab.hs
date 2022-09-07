module Coop.Pab (
  deployAuth,
  mintCoop,
  deployCoop,
  mintCert,
  mintCertRedeemers,
  burnCerts,
  findOutsAtCertV,
  findOutsAtOwnHoldingAa,
  findOutsAtOwnHolding,
  findOutsAtCertVWithCERT,
) where

import Control.Lens ((^.))
import Coop.Pab.Aux (currencyValue, findOutsAt, hasCurrency, hashTxOutRefs, minUtxoAdaValue, mintNft, toDatum)
import Coop.Types (
  AuthDeployment (AuthDeployment, ad'authMp, ad'authorityToken, ad'certMp, ad'certV),
  AuthMpParams (AuthMpParams),
  AuthParams (AuthParams, ap'authTokenCs, ap'certTokenCs),
  CertDatum (CertDatum),
  CertMpParams (CertMpParams),
  CertMpRedeemer (CertMpBurn, CertMpMint),
  CoopDeployment (CoopDeployment, cd'auth),
  CoopPlutus (cp'mkAuthMp, cp'mkCertMp, cp'mkFsMp, cp'mkFsV, cp'mkNftMp),
  FsMpParams (FsMpParams),
  FsVParams (FsVParams),
  cp'certV,
 )
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import Ledger (POSIXTimeRange, applyArguments, getCardanoTxId, scriptCurrencySymbol, validatorHash)
import Ledger.Address (pubKeyHashAddress)
import Ledger.Tx (ChainIndexTxOut, ciTxOutValue)
import Plutus.Contract (Contract, ownFirstPaymentPubKeyHash, submitTxConstraintsWith, waitNSlots)
import Plutus.Contract.Constraints (mintingPolicy, mustMintValueWithRedeemer, mustPayToOtherScript, mustSpendAtLeast, mustSpendPubKeyOutput, mustSpendScriptOutput, otherData, otherScript, ownPaymentPubKeyHash, unspentOutputs)
import Plutus.Contract.Logging (logInfo)
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import Plutus.V1.Ledger.Api (
  CurrencySymbol,
  LedgerBytes (LedgerBytes),
  MintingPolicy (MintingPolicy),
  Redeemer (Redeemer),
  ToData (toBuiltinData),
  TokenName (TokenName),
  TxId,
  TxOutRef,
  Validator (Validator),
  Value,
  toData,
 )
import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.Prelude (Group (inv))
import Test.Plutip.Internal.BotPlutusInterface.Setup ()
import Test.Plutip.Internal.LocalCluster ()

-- TODO: deployCoop aaWallet certRedeemerWallet
deployCoop :: CoopPlutus -> Contract w s Text CoopDeployment
deployCoop coopPlutus = do
  let logI m = logInfo @String ("deployCoop: " <> m)
  logI "Starting"
  authDeployment <- deployAuth coopPlutus
  waitNSlots 5
  (_, (coopCs, coopTn, _)) <- mintCoop coopPlutus
  logI $ "Created COOP instance token ($COOP): " <> show (coopCs, coopTn)
  let fsV = Validator $ applyArguments (cp'mkFsV coopPlutus) [toData (FsVParams coopCs)]
      fsMp =
        MintingPolicy $
          applyArguments
            (cp'mkFsMp coopPlutus)
            [ toData
                ( FsMpParams
                    coopCs
                    (mkValidatorAddress fsV)
                    (authParamsFromDeployment authDeployment)
                )
            ]
  logI "Finished"
  return $ CoopDeployment (coopCs, coopTn) fsMp fsV authDeployment
  where
    authParamsFromDeployment authDeployment =
      AuthParams
        { ap'authTokenCs = scriptCurrencySymbol (ad'authMp authDeployment)
        , ap'certTokenCs = scriptCurrencySymbol (ad'certMp authDeployment)
        }

-- TODO: Parametrise by N $AA tokens
deployAuth :: CoopPlutus -> Contract w s Text AuthDeployment
deployAuth coopPlutus = do
  let logI m = logInfo @String ("deployAuth: " <> m)
  logI "Starting"
  (_, (aaCs, aaTn, _)) <- mintAa coopPlutus
  logI $ "Created Authentication Authority token ($AA): " <> show (aaCs, aaTn)
  let authMpParams = AuthMpParams (aaCs, aaTn) 1
      certMpParams = CertMpParams (aaCs, aaTn) 1 (mkValidatorAddress certV)
      certV = Validator $ cp'certV coopPlutus
      certMp = MintingPolicy $ applyArguments (cp'mkCertMp coopPlutus) [toData certMpParams]
      authMp = MintingPolicy $ applyArguments (cp'mkAuthMp coopPlutus) [toData authMpParams]
  logI "Finished"
  return $ AuthDeployment (aaCs, aaTn) certV certMp authMp

mintAa :: CoopPlutus -> Contract w s Text (TxId, (CurrencySymbol, TokenName, Integer))
mintAa coopPlutus = do
  let logI m = logInfo @String ("mintAa: " <> m)
  logI "Starting"
  mintNft (cp'mkNftMp coopPlutus) 1

mintCoop :: CoopPlutus -> Contract w s Text (TxId, (CurrencySymbol, TokenName, Integer))
mintCoop coopPlutus = do
  let logI m = logInfo @String ("mintCoop: " <> m)
  logI "Starting"
  mintNft (cp'mkNftMp coopPlutus) 1

mintCertRedeemers :: Integer -> CoopPlutus -> Contract w s Text (TxId, (CurrencySymbol, TokenName, Integer))
mintCertRedeemers q coopPlutus = do
  let logI m = logInfo @String ("mintCertRedeemers: " <> m)
  logI "Starting"
  mintNft (cp'mkNftMp coopPlutus) q

mintCert :: (CurrencySymbol, TokenName) -> POSIXTimeRange -> Map TxOutRef ChainIndexTxOut -> CoopDeployment -> Contract w s Text TxId
mintCert redeemerAc validityInterval aaOuts coopDeployment = do
  let logI m = logInfo @String ("mintCert: " <> m)
  logI "Starting"
  self <- ownFirstPaymentPubKeyHash
  let certMp = (ad'certMp . cd'auth) coopDeployment
      certV = (ad'certV . cd'auth) coopDeployment
      certVAddr = validatorHash certV
      aaOrefs = Map.keys aaOuts
      hashedAaOrefs = hashTxOutRefs aaOrefs
      certTn = TokenName hashedAaOrefs
      certId = LedgerBytes hashedAaOrefs
      certCs = scriptCurrencySymbol certMp
      certVal = Value.singleton certCs certTn 1
      certDatum = toDatum $ CertDatum certId validityInterval redeemerAc certCs
      (aaCs, aaTn) = (ad'authorityToken . cd'auth) coopDeployment
      aaVal = Value.singleton aaCs aaTn 1

  let lookups =
        mconcat
          [ mintingPolicy certMp
          , otherScript certV
          , otherData certDatum
          , ownPaymentPubKeyHash self
          , unspentOutputs aaOuts
          ]
      tx =
        mustMintValueWithRedeemer (Redeemer . toBuiltinData $ CertMpMint) certVal
          <> mustPayToOtherScript certVAddr certDatum (certVal <> minUtxoAdaValue)
          <> mconcat (mustSpendPubKeyOutput <$> aaOrefs)
          <> mustSpendAtLeast aaVal -- NOTE: This should be redundant due to the above statement
  tx <- submitTxConstraintsWith @Void lookups tx
  logI "Finished"
  return (getCardanoTxId tx)

burnCerts :: Map TxOutRef ChainIndexTxOut -> Map TxOutRef ChainIndexTxOut -> CoopDeployment -> Contract w s Text TxId
burnCerts certOuts redeemerOuts coopDeployment = do
  let logI m = logInfo @String ("burnCert: " <> m)
  logI "Starting"
  self <- ownFirstPaymentPubKeyHash
  let certMp = (ad'certMp . cd'auth) coopDeployment
      certV = (ad'certV . cd'auth) coopDeployment
      redeemerOrefs = Map.keys redeemerOuts
      certOrefs = Map.keys certOuts
      certCs = scriptCurrencySymbol certMp
      certVal = foldMap (\out -> inv $ currencyValue (out ^. ciTxOutValue) certCs) (toList certOuts)

  let lookups =
        mintingPolicy certMp
          <> otherScript certV
          <> ownPaymentPubKeyHash self
          <> unspentOutputs redeemerOuts
          <> unspentOutputs certOuts

      tx =
        mustMintValueWithRedeemer (Redeemer . toBuiltinData $ CertMpBurn) certVal
          <> mconcat (mustSpendPubKeyOutput <$> redeemerOrefs)
          <> mconcat ((\oref -> mustSpendScriptOutput oref (Redeemer . toBuiltinData $ ())) <$> certOrefs)

  tx <- submitTxConstraintsWith @Void lookups tx
  logI "Finished"
  return (getCardanoTxId tx)

-- | Queries
findOutsAtCertV :: CoopDeployment -> (Value -> CertDatum -> Bool) -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findOutsAtCertV coopDeployment _pred = do
  let logI m = logInfo @String ("findOutsAtCertV: " <> m)

  logI "Starting"

  let certVAddr = (mkValidatorAddress . ad'certV . cd'auth) coopDeployment
  findOutsAt @CertDatum
    certVAddr
    (\_ _ -> True) -- FIXME: (maybe False . pred)

findOutsAtCertVWithCERT :: CoopDeployment -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findOutsAtCertVWithCERT coopDeployment = findOutsAtCertV coopDeployment (\v _ -> hasCurrency v ((scriptCurrencySymbol . ad'certMp . cd'auth) coopDeployment))

findOutsAtOwnHolding :: CurrencySymbol -> TokenName -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findOutsAtOwnHolding cs tn = do
  self <- ownFirstPaymentPubKeyHash
  findOutsAt @Void (pubKeyHashAddress self Nothing) (\v _ -> valueOf v cs tn > 0)

findOutsAtOwnHoldingAa :: CoopDeployment -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findOutsAtOwnHoldingAa coopDeployment = do
  let logI m = logInfo @String ("findOutsAtOwnHoldingAa: " <> m)
  logI "Starting"
  let (aaCs, aaTn) = (ad'authorityToken . cd'auth) coopDeployment
  found <- findOutsAtOwnHolding aaCs aaTn
  logI "Finished"
  return found

-- data FsDatum = FsDatum
--   { fd'fs :: FactStatement
--   , fd'id :: LedgerBytes
--   , fd'description :: FsDescription
--   , fs'gcAfter :: POSIXTime
--   , fs'submitter :: PubKeyHash
--   , fs'fsCs :: CurrencySymbol
--   }
--   deriving stock (Show, Generic, Eq)
--   deriving anyclass (ToJSON, FromJSON)

-- mintFs :: TxOutRef -> TxOutRef -> PubKeyHash -> PubKeyHash -> CoopDeployment -> Contract w s Text TxId
-- mintFs authTokenUtxo certTokenUTxo submitterPkh publisherPkh coopDeployment = do
--   let logI m = logInfo @String ("mintFs: " <> m)
--   logI "Starting"
--   let fsMp = cd'fsMp coopDeployment
--       fsV = cd'fsV coopDeployment
--       fsVAddr = validatorHash fsV
--       fsTn = TokenName . getPubKeyHash $ publisherPkh
--       fsCs = scriptCurrencySymbol fsMp
--       fsVal = Value.singleton fsCs fsTn 1
--       fsDatum = Datum . toBuiltinData $ FsDatum "factstatement" "id" "" 0 submitterPkh fsCs
--       lookups =
--         mconcat
--           [ mintingPolicy fsMp
--           , otherScript fsV
--           , otherData fsDatum
--           ]
--       tx =
--         mconcat
--           [ mustMintValue fsVal
--           , mustSpend
--           , mustBeSignedBy (PaymentPubKeyHash publisherPkh)
--           , mustBeSignedBy (PaymentPubKeyHash submitterPkh)
--           , mustPayToOtherScript fsVAddr fsDatum (fsVal <> minUtxoAdaValue)
--           ]
--   tx <- submitTxConstraintsWith @Void lookups tx
--   logI "Finished"
--   return (getCardanoTxId tx)
