module Coop.Pab (
  deployCoop,
  mintCertRedeemers,
  burnCerts,
  findOutsAtCertV,
  findOutsAtCertVWithCERT,
  findOutsAtHoldingAa,
  burnAuths,
  mkMintCertTrx,
  mkMintAuthTrx,
  mkMintFsTrx,
  mintAuthAndCert,
) where

import Control.Lens ((^.))
import Coop.Pab.Aux (Trx (Trx), currencyValue, findOutsAt, findOutsAt', findOutsAtHolding', findOutsAtHoldingCurrency, hasCurrency, hashTxInputs, interval', minUtxoAdaValue, mkMintOneShotTrx, submitTrx, toDatum, toRedeemer)
import Coop.Types (
  AuthDeployment (AuthDeployment, ad'authMp, ad'authorityAc, ad'certMp, ad'certV),
  AuthMpParams (AuthMpParams),
  AuthMpRedeemer (AuthMpBurn, AuthMpMint),
  AuthParams (AuthParams, ap'authTokenCs, ap'certTokenCs),
  CertDatum (CertDatum),
  CertMpParams (CertMpParams),
  CertMpRedeemer (CertMpBurn, CertMpMint),
  CoopDeployment (CoopDeployment, cd'auth, cd'fsMp, cd'fsV),
  CoopPlutus (cp'fsV, cp'mkAuthMp, cp'mkCertMp, cp'mkFsMp, cp'mkOneShotMp),
  FsDatum,
  FsMpParams (FsMpParams),
  FsMpRedeemer (FsMpMint),
  cp'certV,
 )
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import Ledger (PaymentPubKeyHash, applyArguments, getCardanoTxId, interval)
import Ledger.Tx (ChainIndexTxOut, ciTxOutValue)
import Plutus.Contract (Contract, currentTime, ownFirstPaymentPubKeyHash, submitTxConstraintsWith, throwError)
import Plutus.Contract.Constraints (mustBeSignedBy, mustMintValueWithRedeemer, mustPayToOtherScript, mustPayToPubKey, mustSpendPubKeyOutput, mustSpendScriptOutput, mustValidateIn, otherData, ownPaymentPubKeyHash, plutusV2MintingPolicy, plutusV2OtherScript, unspentOutputs)
import Plutus.Contract.Logging (logInfo)
import Plutus.Script.Utils.V2.Address (mkValidatorAddress)
import Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol, validatorHash)
import Plutus.V1.Ledger.Value (
  AssetClass,
  assetClass,
 )
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api (
  Extended (Finite, PosInf),
  LedgerBytes (LedgerBytes),
  MintingPolicy (MintingPolicy),
  POSIXTime,
  POSIXTimeRange,
  TokenName (TokenName),
  TxId,
  TxOutRef,
  Validator (Validator),
  Value,
  toData,
 )
import PlutusTx.Prelude (Group (inv))
import Test.Plutip.Internal.BotPlutusInterface.Setup ()
import Test.Plutip.Internal.LocalCluster ()
import Text.Printf (printf)

deployCoop :: CoopPlutus -> PaymentPubKeyHash -> Integer -> Integer -> Contract w s Text CoopDeployment
deployCoop coopPlutus aaWallet atLeastAaQ aaQToMint = do
  let logI m = logInfo @String ("deployCoop: " <> m)
  logI "Starting"

  bool
    (throwError "deployCoop: Must specify more or equal $AA tokens to mint than is required to authorize $AUTH/$CERT")
    (return ())
    (atLeastAaQ >= aaQToMint)

  self <- ownFirstPaymentPubKeyHash

  outs <- findOutsAt' @Void self (\_ _ -> True)
  (aaOut, coopOut) <- case Map.toList outs of
    o1 : o2 : _ -> return (o1, o2)
    _ -> throwError "deployCoop: Not enough outputs found to use for making $ONE-SHOTs"

  logI $ printf "Using TxOutRef %s for minting $AA one-shot tokens" (show $ fst aaOut)
  logI $ printf "Using TxOutRef %s for minting $COOP one-shot tokens" (show $ fst coopOut)

  let mkOneShotMp = cp'mkOneShotMp coopPlutus
      (mintAaTrx, aaAc) = mkMintOneShotTrx self aaWallet aaOut mkOneShotMp aaQToMint
      (mintCoopTrx, coopAc) = mkMintOneShotTrx self self coopOut mkOneShotMp 1

  submitTrx @Void (mintAaTrx <> mintCoopTrx)
  logI $ printf "Created the $COOP instance token %s and sent it to God Wallet %s" (show coopAc) (show self)
  logI $ printf "Created %d $AA (Authentication Authority) tokens %s and sent them to AA Wallet %s" atLeastAaQ (show aaAc) (show aaWallet)

  let authDeployment = mkAuthDeployment coopPlutus aaAc atLeastAaQ
      coopDeployment = mkCoopDeployment coopPlutus coopAc authDeployment
  logI "Finished"
  return coopDeployment

mkAuthDeployment :: CoopPlutus -> AssetClass -> Integer -> AuthDeployment
mkAuthDeployment coopPlutus aaAc atLeastAaQ =
  let authMpParams = AuthMpParams aaAc atLeastAaQ
      certMpParams = CertMpParams aaAc atLeastAaQ (mkValidatorAddress certV)
      certV = Validator $ cp'certV coopPlutus
      certMp = MintingPolicy $ applyArguments (cp'mkCertMp coopPlutus) [toData certMpParams]
      authMp = MintingPolicy $ applyArguments (cp'mkAuthMp coopPlutus) [toData authMpParams]
   in AuthDeployment aaAc certV certMp authMp

mkCoopDeployment :: CoopPlutus -> AssetClass -> AuthDeployment -> CoopDeployment
mkCoopDeployment coopPlutus coopAc authDeployment =
  let fsV = Validator (cp'fsV coopPlutus)
      fsMp =
        MintingPolicy $
          applyArguments
            (cp'mkFsMp coopPlutus)
            [ toData
                ( FsMpParams
                    coopAc
                    (mkValidatorAddress fsV)
                    (authParamsFromDeployment authDeployment)
                )
            ]
   in CoopDeployment coopAc fsMp fsV authDeployment
  where
    authParamsFromDeployment authDeployment =
      AuthParams
        { ap'authTokenCs = scriptCurrencySymbol (ad'authMp authDeployment)
        , ap'certTokenCs = scriptCurrencySymbol (ad'certMp authDeployment)
        }

mintCertRedeemers :: CoopPlutus -> Integer -> Contract w s Text AssetClass
mintCertRedeemers coopPlutus q = do
  let logI m = logInfo @String ("mintCertR: " <> m)
  logI "Starting"
  self <- ownFirstPaymentPubKeyHash

  outs <- findOutsAt' @Void self (\_ _ -> True)
  out <- case reverse . Map.toList $ outs of -- FIXME: If I don't shuffle this I get InsuficientCollateral
    o1 : _ -> return o1
    _ -> throwError "mintCertR: Not enough outputs found to use for making $ONE-SHOTs"

  let mkOneShotMp = cp'mkOneShotMp coopPlutus
      (mintCertRTrx, certRAc) = mkMintOneShotTrx self self out mkOneShotMp q

  submitTrx @Void mintCertRTrx
  logI $ printf "Created $CertR redeemer token for redeeming $CERT outputs @CertV: %s" (show certRAc)
  logI "Finished"
  return certRAc

mkMintCertTrx :: CoopDeployment -> PaymentPubKeyHash -> AssetClass -> POSIXTimeRange -> Map TxOutRef ChainIndexTxOut -> (Trx i o a, AssetClass)
mkMintCertTrx coopDeployment self redeemerAc validityInterval aaOuts =
  let certMp = (ad'certMp . cd'auth) coopDeployment
      certV = (ad'certV . cd'auth) coopDeployment
      certVAddr = validatorHash certV
      aaOrefs = Map.keys aaOuts
      certId = hashTxInputs aaOuts
      certTn = TokenName certId
      certCs = scriptCurrencySymbol certMp
      certVal = Value.singleton certCs certTn 1
      certDatum = toDatum $ CertDatum (LedgerBytes certId) validityInterval redeemerAc
      lookups =
        mconcat
          [ plutusV2MintingPolicy certMp
          , plutusV2OtherScript certV
          , otherData certDatum
          , ownPaymentPubKeyHash self
          , unspentOutputs aaOuts
          ]
      constraints =
        mustMintValueWithRedeemer (toRedeemer CertMpMint) certVal
          <> mustPayToOtherScript certVAddr certDatum (certVal <> minUtxoAdaValue)
          <> mconcat (mustSpendPubKeyOutput <$> aaOrefs)
   in (Trx lookups constraints, assetClass certCs certTn)

burnCerts :: CoopDeployment -> AssetClass -> Contract w s Text TxId
burnCerts coopDeployment certRdmrAc = do
  let logI m = logInfo @String ("burnCerts: " <> m)
  logI "Starting"
  self <- ownFirstPaymentPubKeyHash
  certRdmrOuts <- findOutsAtHolding' self certRdmrAc
  certOuts <- findOutsAtHoldingCurrency (mkValidatorAddress . ad'certV . cd'auth $ coopDeployment) (scriptCurrencySymbol . ad'certMp . cd'auth $ coopDeployment)
  logI $ printf "Found %d $CERT ouputs at @CertV" (length certOuts)
  bool
    (throwError "burnCerts: There should be some $CERT inputs")
    (pure ())
    $ not (null certOuts)

  now <- currentTime
  let certMp = (ad'certMp . cd'auth) coopDeployment
      certV = (ad'certV . cd'auth) coopDeployment
      certRdmdrOrefs = Map.keys certRdmrOuts
      certOrefs = Map.keys certOuts
      certCs = scriptCurrencySymbol certMp
      certVal = foldMap (\out -> inv $ currencyValue (out ^. ciTxOutValue) certCs) (toList certOuts)
      timeRange = interval' (Finite now) PosInf

  let lookups =
        plutusV2MintingPolicy certMp
          <> plutusV2OtherScript certV
          <> ownPaymentPubKeyHash self
          <> unspentOutputs certRdmrOuts
          <> unspentOutputs certOuts

      tx =
        mustMintValueWithRedeemer (toRedeemer CertMpBurn) certVal
          <> mconcat (mustSpendPubKeyOutput <$> certRdmdrOrefs)
          <> mconcat ((\oref -> mustSpendScriptOutput oref (toRedeemer ())) <$> certOrefs)
          <> mustValidateIn timeRange

  tx <- submitTxConstraintsWith @Void lookups tx
  logI "Finished"
  return (getCardanoTxId tx)

mkMintAuthTrx :: CoopDeployment -> PaymentPubKeyHash -> [PaymentPubKeyHash] -> Integer -> Map TxOutRef ChainIndexTxOut -> (Trx i o a, AssetClass)
mkMintAuthTrx coopDeployment self authWallets authQEach aaOuts =
  let authMp = (ad'authMp . cd'auth) coopDeployment
      aaOrefs = Map.keys aaOuts
      authId = hashTxInputs aaOuts
      authTn = TokenName authId
      authCs = scriptCurrencySymbol authMp
      authValEach = Value.singleton authCs authTn authQEach
      authValTotal = Value.singleton authCs authTn (authQEach * (fromIntegral . length $ authWallets))
      lookups =
        plutusV2MintingPolicy authMp
          <> ownPaymentPubKeyHash self
          <> unspentOutputs aaOuts
      constraints =
        mustMintValueWithRedeemer (toRedeemer AuthMpMint) authValTotal
          <> mconcat ((`mustPayToPubKey` (authValEach <> minUtxoAdaValue)) <$> authWallets)
          <> mconcat (mustSpendPubKeyOutput <$> aaOrefs)
   in (Trx lookups constraints, assetClass authCs authTn)

mintAuthAndCert ::
  CoopDeployment ->
  [PaymentPubKeyHash] ->
  Integer ->
  AssetClass ->
  POSIXTime ->
  POSIXTime ->
  Contract w s Text (AssetClass, AssetClass)
mintAuthAndCert coopDeployment authWallets nAuthTokensPerWallet certRdmrAc from to = do
  let logI m = logInfo @String ("mintAuthAndCert: " <> m)
  logI $ printf "Minting $AUTH and $CERT tokens with %d $AUTH wallets distributing %d $AUTH tokens to each with validity interval %s " (length authWallets) nAuthTokensPerWallet (show $ interval from to)
  now <- currentTime
  logI $ printf "Current time is %s" (show now)
  self <- ownFirstPaymentPubKeyHash
  aaOuts <- findOutsAtHoldingAa self coopDeployment
  let validityInterval = interval from to
  let (mintAuthTrx, authAc) = mkMintAuthTrx coopDeployment self authWallets nAuthTokensPerWallet aaOuts
      (mintCertTrx, certAc) = mkMintCertTrx coopDeployment self certRdmrAc validityInterval aaOuts
  submitTrx @Void (mintAuthTrx <> mintCertTrx)
  logI "Finished"
  return (certAc, authAc)

mkBurnAuthsTrx :: CoopDeployment -> PaymentPubKeyHash -> Map TxOutRef ChainIndexTxOut -> Trx i o a
mkBurnAuthsTrx coopDeployment self authOuts = do
  let authMp = (ad'authMp . cd'auth) coopDeployment
      authCs = scriptCurrencySymbol authMp
      authOrefs = Map.keys authOuts
      authVal = foldMap (\out -> inv $ currencyValue (out ^. ciTxOutValue) authCs) (toList authOuts)

      lookups =
        plutusV2MintingPolicy authMp
          <> ownPaymentPubKeyHash self
          <> unspentOutputs authOuts

      constraints =
        mustMintValueWithRedeemer (toRedeemer AuthMpBurn) authVal
          <> mconcat (mustSpendPubKeyOutput <$> authOrefs)
   in Trx lookups constraints

burnAuths :: CoopDeployment -> PaymentPubKeyHash -> Map TxOutRef ChainIndexTxOut -> Contract w s Text ()
burnAuths coopDeployment self authOuts = do
  let logI m = logInfo @String ("burnAuths: " <> m)
  logI "Starting"
  let trx = mkBurnAuthsTrx coopDeployment self authOuts
  submitTrx @Void trx
  logI "Finished"

-- | Queries
findOutsAtCertV :: CoopDeployment -> (Value -> CertDatum -> Bool) -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findOutsAtCertV coopDeployment pred = do
  let logI m = logInfo @String ("findOutsAtCertV: " <> m)

  logI "Starting"

  let certVAddr = (mkValidatorAddress . ad'certV . cd'auth) coopDeployment
  findOutsAt @CertDatum
    certVAddr
    (maybe False . pred)

findOutsAtCertVWithCERT :: CoopDeployment -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findOutsAtCertVWithCERT coopDeployment = findOutsAtCertV coopDeployment (\v _ -> hasCurrency v ((scriptCurrencySymbol . ad'certMp . cd'auth) coopDeployment))

findOutsAtHoldingAa :: PaymentPubKeyHash -> CoopDeployment -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findOutsAtHoldingAa wallet coopDeployment = do
  let logI m = logInfo @String ("findOutsAtHoldingAa: " <> m)
  logI "Starting"
  let aaAc = (ad'authorityAc . cd'auth) coopDeployment
  found <- findOutsAtHolding' wallet aaAc
  logI "Finished"
  return found

-- TODO
mkMintFsTrx :: CoopDeployment -> PaymentPubKeyHash -> POSIXTimeRange -> FsDatum -> (TxOutRef, ChainIndexTxOut) -> ((TxOutRef, ChainIndexTxOut), CertDatum) -> PaymentPubKeyHash -> (Trx i o a, AssetClass)
mkMintFsTrx coopDeployment self trxValidRange fsDatum authOut (certOut, certDatum) submitterPpkh = do
  let fsMp = cd'fsMp coopDeployment
      fsV = cd'fsV coopDeployment
      fsVAddr = validatorHash fsV
      fsCs = scriptCurrencySymbol fsMp
      fsTn = TokenName . hashTxInputs $ Map.fromList [authOut]
      fsVal = Value.singleton fsCs fsTn 1
      certV = ad'certV . cd'auth $ coopDeployment
      lookups =
        plutusV2MintingPolicy fsMp
          <> plutusV2OtherScript fsV
          <> plutusV2OtherScript certV
          <> otherData (toDatum fsDatum)
          <> otherData (toDatum certDatum) -- TODO: Debugging, remove
          <> unspentOutputs (Map.fromList [certOut])
          <> ownPaymentPubKeyHash submitterPpkh

      constraints =
        mustMintValueWithRedeemer (toRedeemer FsMpMint) fsVal
          <> mustSpendPubKeyOutput (fst authOut)
          <> mustSpendScriptOutput (fst certOut) (toRedeemer ()) -- FIXME: We need mustReferenceScriptOutput
          <> mustBeSignedBy self
          <> mustBeSignedBy submitterPpkh
          <> mustPayToOtherScript fsVAddr (toDatum fsDatum) (fsVal <> minUtxoAdaValue)
          <> mustValidateIn trxValidRange
      -- TODO[Andrea]: add fee output?
      mintFsTrx =
        mkBurnAuthsTrx coopDeployment self (Map.fromList [authOut])
          <> Trx lookups constraints
   in (mintFsTrx, assetClass fsCs fsTn)
