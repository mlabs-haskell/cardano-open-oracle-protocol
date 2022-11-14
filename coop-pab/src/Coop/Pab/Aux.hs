{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Coop.Pab.Aux (
  Trx (..),
  datumFromTxOut,
  loadCoopPlutus,
  runBpi,
  hashTxInputs,
  DeployMode (..),
  minUtxoAdaValue,
  mkMintOneShotTrx,
  hasCurrency,
  currencyValue,
  findOutsAt,
  toDatum,
  fromDatum,
  findOutsAtHolding,
  ciValueOf,
  toRedeemer,
  submitTrx,
  findOutsAt',
  findOutsAtHolding',
  interval',
  findOutsAtHoldingCurrency,
  findOutsAtHoldingCurrency',
  datumFromTxOutOrFail,
  deplCertVAddress,
  deplFsVAddress,
  deplFsVHash,
  deplCertCs,
  deplAuthCs,
  deplAuthMp,
  deplFsCs,
  submitTrx',
) where

import BotPlutusInterface.Constraints (submitBpiTxConstraintsWith)
import BotPlutusInterface.Constraints qualified as BpiConstr
import BotPlutusInterface.Contract (runContract)
import BotPlutusInterface.Types (CollateralVar (CollateralVar), ContractEnvironment (ContractEnvironment), ContractState (ContractState), PABConfig, ceCollateral, ceContractInstanceId, ceContractLogs, ceContractState, ceContractStats, cePABConfig)
import Control.Applicative ((<|>))
import Control.Concurrent.STM (newTVarIO)
import Control.Lens ((^.), (^?))
import Control.Monad (filterM)
import Coop.Types (AuthDeployment (ad'authMp, ad'certMp, ad'certV), CoopDeployment (cd'auth, cd'fsMp, cd'fsV), CoopPlutus)
import Crypto.Hash (Blake2b_256 (Blake2b_256), hashWith)
import Data.Aeson (ToJSON, decodeFileStrict)
import Data.ByteArray (convert)
import Data.ByteString (cons)
import Data.Dynamic (Typeable)
import Data.Kind (Type)
import Data.List (sort)
import Data.Map (Map, fromList)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID.V4 qualified as UUID
import Data.Void (Void)
import Ledger (ChainIndexTxOut, PaymentPubKeyHash, applyArguments, ciTxOutPublicKeyDatum, ciTxOutScriptDatum, ciTxOutValue, datumInDatumFromQuery, getCardanoTxId, pubKeyHashAddress)
import Ledger.Ada (lovelaceValueOf)
import Ledger.Typed.Scripts (RedeemerType, ValidatorTypes (DatumType))
import Plutus.Contract (AsContractError, Contract, ContractInstanceId, awaitTxConfirmed, logInfo, throwError, utxosAt)
import Plutus.Contract.Constraints (ScriptLookups, TxConstraints, mustMintValue, mustPayToPubKey, mustSpendPubKeyOutput, ownPaymentPubKeyHash, plutusV2MintingPolicy, unspentOutputs)
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import Plutus.Script.Utils.V2.Address (mkValidatorAddress)
import Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol, validatorHash)
import Plutus.V1.Ledger.Value (AssetClass (unAssetClass), assetClass, valueOf)
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api (Address, BuiltinByteString, CurrencySymbol, Datum (Datum, getDatum), Extended, FromData (fromBuiltinData), Interval (Interval), LowerBound (LowerBound), MintingPolicy (MintingPolicy), Redeemer (Redeemer), Script, ToData, TokenName (TokenName), TxId (getTxId), TxOutRef (txOutRefId, txOutRefIdx), UpperBound (UpperBound), ValidatorHash, Value (Value), fromBuiltin, toBuiltinData, toData)
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
  collateral <- CollateralVar <$> newTVarIO Nothing

  let contractEnv =
        ContractEnvironment
          { cePABConfig = pabConf
          , ceContractState = contractState
          , ceContractInstanceId = contractInstanceID
          , ceContractStats = contractStats
          , ceContractLogs = contractLogs
          , ceCollateral = collateral
          }
  Prelude.putStr $ show contractEnv
  result <- runContract contractEnv contract
  pure (contractInstanceID, result)

minUtxoAdaValue :: Value
minUtxoAdaValue = lovelaceValueOf 2_000_000

-- | Checks if Value has tokens of CurrencySymbol
hasCurrency :: Value -> CurrencySymbol -> Bool
hasCurrency (Value vals) cs = AssocMap.member cs vals

currencyValue :: Value -> CurrencySymbol -> Value
currencyValue (Value vals) cs = maybe mempty (Value . AssocMap.singleton cs) $ AssocMap.lookup cs vals

-- | Makes a OneShot minting transaction that's validated by `coop-plutus`.Coop.Plutus.Aux.mkOneShotMp
mkMintOneShotTrx :: PaymentPubKeyHash -> PaymentPubKeyHash -> (TxOutRef, ChainIndexTxOut) -> Script -> Integer -> (Trx i o a, AssetClass)
mkMintOneShotTrx self toWallet out@(oref, _) mkOneShotMp q =
  let oneShotTn = TokenName . hashTxInputs $ Map.fromList [out]
      oneShotMp = MintingPolicy $ applyArguments mkOneShotMp [toData q, toData oneShotTn, toData oref]
      oneShotCs = scriptCurrencySymbol oneShotMp
      val = Value.singleton oneShotCs oneShotTn q
      lookups =
        plutusV2MintingPolicy oneShotMp
          <> unspentOutputs (fromList [out])
          <> ownPaymentPubKeyHash self
      constraints =
        mustMintValue val
          <> mustSpendPubKeyOutput oref
          <> mustPayToPubKey toWallet (val <> minUtxoAdaValue)
   in (Trx lookups constraints [], assetClass oneShotCs oneShotTn)

{- | Hashes transaction inputs blake2b_256 on the concatenation of id:ix (used for onchain uniqueness)

TODO: Consolidate with the same `coop-plutus`.Coop.Plutus.Aux.hashTxInputs in a shared location
-}
hashTxInputs :: Map TxOutRef ChainIndexTxOut -> BuiltinByteString
hashTxInputs inputs =
  let orefs = [oref | (oref, _) <- Map.toList inputs]
      sortedOrefs = sort orefs
      ixs = fmap (fromInteger . txOutRefIdx) sortedOrefs
      txIds = fmap (fromBuiltin . getTxId . txOutRefId) sortedOrefs
      hashedOref = convert @_ @BuiltinByteString . hashWith Blake2b_256 . mconcat $ zipWith cons ixs txIds
   in hashedOref

type CoopContract w s = Contract w s Text

-- | Tries to parse Data from ChainIndexTxOut which is surprisingly complicated
datumFromTxOut :: forall (a :: Type) w s. Typeable a => FromData a => ChainIndexTxOut -> CoopContract w s (Maybe a)
datumFromTxOut out = return $ tryPkDat out <|> tryScDat out
  where
    tryPkDat o = do
      mayPkDat <- o ^? ciTxOutPublicKeyDatum
      (_, pkDat) <- mayPkDat
      d <- pkDat ^? datumInDatumFromQuery
      fromDatum d
    tryScDat o = do
      (_, scDat) <- o ^? ciTxOutScriptDatum
      d <- scDat ^? datumInDatumFromQuery
      fromDatum d

datumFromTxOutOrFail :: (FromData b, Typeable b) => ChainIndexTxOut -> Text -> Contract w s Text b
datumFromTxOutOrFail out msg = do
  mayDat <- datumFromTxOut out
  maybe (throwError msg) return mayDat

deplCertVAddress :: CoopDeployment -> Address
deplCertVAddress = mkValidatorAddress . ad'certV . cd'auth

deplFsVAddress :: CoopDeployment -> Address
deplFsVAddress depl =
  let fsV = cd'fsV depl in mkValidatorAddress fsV

deplFsVHash :: CoopDeployment -> ValidatorHash
deplFsVHash depl =
  let fsV = cd'fsV depl in validatorHash fsV

deplCertCs :: CoopDeployment -> CurrencySymbol
deplCertCs = scriptCurrencySymbol . ad'certMp . cd'auth

deplAuthCs :: CoopDeployment -> CurrencySymbol
deplAuthCs = scriptCurrencySymbol . ad'authMp . cd'auth

deplAuthMp :: CoopDeployment -> MintingPolicy
deplAuthMp = ad'authMp . cd'auth

deplFsCs :: CoopDeployment -> CurrencySymbol
deplFsCs = scriptCurrencySymbol . cd'fsMp

findOutsAt :: forall a w s. Typeable a => FromData a => Address -> (Value -> Maybe a -> Bool) -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findOutsAt addr p = do
  let logI m = logInfo @String ("findOutsAt: " <> m)
  logI "Starting"

  outs <- utxosAt addr
  found <-
    filterM
      ( \(_, out) -> do
          dat <- datumFromTxOut @a out
          return $ p (out ^. ciTxOutValue) dat
      )
      (Map.toList outs)

  logI $ printf "Found %d TxOuts @%s" (length found) (show addr)
  return $ Map.fromList found

findOutsAt' :: forall a w s. (Typeable a, FromData a) => PaymentPubKeyHash -> (Value -> Maybe a -> Bool) -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findOutsAt' ppkh = findOutsAt @a (pubKeyHashAddress ppkh Nothing)

findOutsAtHoldingCurrency :: Address -> CurrencySymbol -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findOutsAtHoldingCurrency addr cs = findOutsAt @Void addr (\v _ -> hasCurrency v cs)

findOutsAtHoldingCurrency' :: PaymentPubKeyHash -> CurrencySymbol -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findOutsAtHoldingCurrency' wallet = findOutsAtHoldingCurrency (pubKeyHashAddress wallet Nothing)

findOutsAtHolding :: Address -> AssetClass -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findOutsAtHolding addr ac = do
  let (cs, tn) = unAssetClass ac
  findOutsAt @Void addr (\v _ -> valueOf v cs tn > 0)

findOutsAtHolding' :: PaymentPubKeyHash -> AssetClass -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findOutsAtHolding' wallet = findOutsAtHolding (pubKeyHashAddress wallet Nothing)

toDatum :: ToData a => a -> Datum
toDatum = Datum . toBuiltinData

toRedeemer :: ToData a => a -> Redeemer
toRedeemer = Redeemer . toBuiltinData

fromDatum :: FromData a => Datum -> Maybe a
fromDatum = fromBuiltinData . getDatum

ciValueOf :: AssetClass -> ChainIndexTxOut -> Integer
ciValueOf ac out = let (cs, tn) = unAssetClass ac in valueOf (out ^. ciTxOutValue) cs tn

-- | Trx utilities
data Trx i o a = Trx (ScriptLookups a) (TxConstraints i o) [BpiConstr.BpiConstraint]

instance Semigroup (Trx i o a) where
  Trx l c b <> Trx l' c' b' = Trx (l <> l') (c <> c') (b <> b')

instance Monoid (Trx i o a) where
  mempty = Trx mempty mempty mempty

submitTrx :: forall a e w s. (FromData (DatumType a), ToData (RedeemerType a), ToData (DatumType a), AsContractError e) => Trx (RedeemerType a) (DatumType a) a -> Contract w s e TxId
submitTrx (Trx lookups constraints bpiConstraints) = do
  tx <- submitBpiTxConstraintsWith lookups constraints bpiConstraints
  awaitTxConfirmed (getCardanoTxId tx)
  return $ getCardanoTxId tx

submitTrx' :: forall a e w s. (FromData (DatumType a), ToData (RedeemerType a), ToData (DatumType a), AsContractError e) => Trx (RedeemerType a) (DatumType a) a -> Contract w s e TxId
submitTrx' (Trx lookups constraints bpiConstraints) = do
  tx <- submitBpiTxConstraintsWith lookups constraints bpiConstraints
  return $ getCardanoTxId tx

-- | Creates an interval with Extended bounds
interval' :: forall a. Extended a -> Extended a -> Interval a
interval' from to = Interval (LowerBound from False) (UpperBound to False)

-- | ProtoCardano instances
instance MonadFail (Contract w s Text) where
  fail = throwError . Text.pack

instance MonadFail (Contract w s String) where
  fail = throwError
