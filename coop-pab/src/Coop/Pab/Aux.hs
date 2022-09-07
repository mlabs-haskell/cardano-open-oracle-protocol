module Coop.Pab.Aux (loadCoopPlutus, runBpi, DeployMode (..), minUtxoAdaValue, mintNft, hasCurrency, currencyValue, makeCollateralOuts, findOutsAt, toDatum, fromDatum, hashTxOutRefs, findOutsAtHolding, testDataRoundtrip, testDataRoundtrip', ciValueOf) where

import BotPlutusInterface.Contract (runContract)
import BotPlutusInterface.Types (ContractEnvironment (ContractEnvironment), ContractState (ContractState), PABConfig, ceContractInstanceId, ceContractLogs, ceContractState, ceContractStats, cePABConfig)
import Control.Concurrent.STM (newTVarIO)
import Control.Lens ((^.), (^?))
import Control.Monad (filterM)
import Coop.Types (CoopPlutus)
import Crypto.Hash (SHA3_256 (SHA3_256), hashWith)
import Data.Aeson (ToJSON, decodeFileStrict)
import Data.Bool (bool)
import Data.ByteArray (convert)
import Data.ByteString (ByteString, cons)
import Data.Data (Typeable)
import Data.Kind (Type)
import Data.Map (Map, keys)
import Data.Map qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Typeable (typeRep)
import Data.UUID.V4 qualified as UUID
import Data.Void (Void)
import Ledger (ChainIndexTxOut, PaymentPubKeyHash, applyArguments, ciTxOutDatum, ciTxOutValue, getCardanoTxId, pubKeyHashAddress)
import Ledger.Ada (lovelaceValueOf)
import Ledger.Value (Value (Value), isAdaOnlyValue)
import Plutus.Contract (Contract, ContractInstanceId, datumFromHash, logInfo, ownFirstPaymentPubKeyHash, submitTxConstraintsWith, throwError, utxosAt, waitNSlots)
import Plutus.Contract.Constraints (mintingPolicy, mustMintValue, mustPayToOtherScript, mustPayToPubKey, mustSpendPubKeyOutput, otherData, otherScript, ownPaymentPubKeyHash, unspentOutputs)
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import Plutus.Script.Utils.V1.Scripts (scriptCurrencySymbol, validatorHash)
import Plutus.V1.Ledger.Api (Address, BuiltinByteString, CurrencySymbol, Datum (Datum, getDatum), FromData (fromBuiltinData), MintingPolicy (MintingPolicy), Script, ToData, TokenName (TokenName), TxId (getTxId), TxOutRef (txOutRefId, txOutRefIdx), Validator, adaSymbol, adaToken, fromBuiltin, toBuiltin, toBuiltinData, toData)
import Plutus.V1.Ledger.Value (AssetClass (unAssetClass), assetClass, valueOf)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.AssocMap qualified as AssocMap
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

hasCurrency :: Value -> CurrencySymbol -> Bool
hasCurrency (Value vals) cs = AssocMap.member cs vals

currencyValue :: Value -> CurrencySymbol -> Value
currencyValue (Value vals) cs = maybe mempty (Value . AssocMap.singleton cs) $ AssocMap.lookup cs vals

mintNft :: PaymentPubKeyHash -> PaymentPubKeyHash -> Script -> Integer -> Contract w s Text (TxId, (AssetClass, Integer))
mintNft self toWallet mkNftMp q = do
  let logI m = logInfo @String ("mintNft: " <> m)
  logI "Starting"
  adaOnlyOuts <- findOutsAtHoldingOnlyAda (pubKeyHashAddress self Nothing) (const True)
  case keys adaOnlyOuts of
    [] -> do
      throwError "mintNft: no utxo found"
    oref : _ -> do
      logI $ "Using oref " <> show oref
      let nftTn = TokenName . hashTxOutRefs $ [oref]
          nftMp = MintingPolicy $ applyArguments mkNftMp [toData q, toData nftTn, toData oref]
          nftCs = scriptCurrencySymbol nftMp
          val = Value.singleton nftCs nftTn q
          lookups =
            mintingPolicy nftMp
              <> unspentOutputs adaOnlyOuts
              <> ownPaymentPubKeyHash self
          tx =
            mustMintValue val
              <> mustSpendPubKeyOutput oref
              <> mustPayToPubKey toWallet (val <> minUtxoAdaValue)

      tx <- submitTxConstraintsWith @Void lookups tx
      logI $ printf "NFT minted %s and sent to %s" (show val) (show toWallet)
      logI "Finished"
      return (getCardanoTxId tx, (assetClass nftCs nftTn, q))

-- FIXME: Sort orefs to match the onchain order
-- TODO: Switch to using blake
hashTxOutRefs :: [TxOutRef] -> BuiltinByteString
hashTxOutRefs orefs =
  let ixs = fmap (fromInteger . txOutRefIdx) orefs
      txIds = fmap (fromBuiltin . getTxId . txOutRefId) orefs
      hashedOref = convert @_ @ByteString . hashWith SHA3_256 . mconcat $ zipWith cons ixs txIds
   in toBuiltin hashedOref

makeCollateralOuts :: PaymentPubKeyHash -> Integer -> Integer -> Contract w s Text TxId
makeCollateralOuts self n lovelace = do
  let logI m = logInfo @String ("makeCollateralOuts: " <> m)
  logI "Starting"
  adaOnlyOuts <- findOutsAtHoldingOnlyAda (pubKeyHashAddress self Nothing) (>= lovelace)
  let adaOnlyOutsToMake = fromIntegral n - length adaOnlyOuts
  let lookups = ownPaymentPubKeyHash self
      tx = mconcat $ replicate adaOnlyOutsToMake $ mustPayToPubKey self (lovelaceValueOf lovelace)
  tx <- submitTxConstraintsWith @Void lookups tx
  logI "Finished"
  return $ getCardanoTxId tx

datumFromTxOut :: forall w s (a :: Type). Typeable a => FromData a => ChainIndexTxOut -> Contract w s Text (Maybe a)
datumFromTxOut out =
  let logI m = logInfo @String ("datumFromTxOut: " <> m)
   in maybe
        (logI "Datum not present in the output" >> pure Nothing)
        ( \hashOrDatum -> do
            dat <-
              either
                ( \h -> do
                    logI "Got datum hash"
                    mayDat <- datumFromHash h
                    maybe (throwError "datumFromHash failed") pure mayDat
                )
                (\d -> logI "Got inlined datum" >> pure d)
                hashOrDatum
            maybe
              (logI ("fromDatum failed: " <> show (typeRep (Proxy @a)) <> " is not: " <> show dat) >> pure Nothing)
              (pure . Just)
              (fromDatum dat)
        )
        (out ^? ciTxOutDatum)

findOutsAt :: Typeable a => FromData a => Address -> (Value -> Maybe a -> Bool) -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findOutsAt addr pred = do
  let logI m = logInfo @String ("findOutsAt: " <> m)
  logI "Starting"

  outs <- utxosAt addr
  found <-
    filterM
      ( \(_, out) -> do
          dat <- datumFromTxOut out
          return $ pred (out ^. ciTxOutValue) dat
      )
      (Map.toList outs)

  logI $ "Found " <> (show . length $ found) <> " TxOuts @" <> show addr
  return $ Map.fromList found

findOutsAtHolding :: PaymentPubKeyHash -> AssetClass -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findOutsAtHolding wallet ac = do
  let (cs, tn) = unAssetClass ac
  findOutsAt @Void (pubKeyHashAddress wallet Nothing) (\v _ -> valueOf v cs tn > 0)

findOutsAtHoldingOnlyAda :: Address -> (Integer -> Bool) -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findOutsAtHoldingOnlyAda addr pred = do
  let logI m = logInfo @String ("findOutsAtHoldingOnlyAda: " <> m)
  logI "Starting"

  found <- findOutsAt @Void addr (\v _ -> isAdaOnlyValue v && pred (valueOf v adaSymbol adaToken))

  logI "Finished"
  return found

toDatum :: ToData a => a -> Datum
toDatum = Datum . toBuiltinData

fromDatum :: FromData a => Datum -> Maybe a
fromDatum = fromBuiltinData . getDatum

testDataRoundtrip :: (Typeable a, FromData a, ToData a, Eq a) => Validator -> a -> Contract w s Text TxId
testDataRoundtrip val x = do
  let logI m = logInfo @String ("testDataRoundtrip: " <> m)
  self <- ownFirstPaymentPubKeyHash

  let datum = toDatum x
      lookups =
        mconcat
          [ otherScript val
          , otherData datum
          , ownPaymentPubKeyHash self
          ]
      tx = mustPayToOtherScript (validatorHash val) datum minUtxoAdaValue
  logI $ "Sending datum: " <> show datum
  tx <- submitTxConstraintsWith @Void lookups tx
  waitNSlots 30
  found <- findOutsAt (mkValidatorAddress val) (\_ mayX -> mayX == Just x)
  bool
    (logI "Found the datum that was sent")
    (throwError "Must find the datum that was sent")
    $ found == mempty
  logI "Finished"
  return $ getCardanoTxId tx

testDataRoundtrip' :: (Typeable a, FromData a, ToData a, Eq a) => a -> Bool
testDataRoundtrip' x = let datum = toDatum x; mayX = fromDatum datum in mayX == Just x

ciValueOf :: AssetClass -> ChainIndexTxOut -> Integer
ciValueOf ac out = let (cs, tn) = unAssetClass ac in valueOf (out ^. ciTxOutValue) cs tn

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
