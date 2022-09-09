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
) where

import Control.Lens ((^.))
import Coop.Pab.Aux (Trx (Trx), currencyValue, findOutsAt, findOutsAt', findOutsAtHolding', hasCurrency, hashTxOutRefs, minUtxoAdaValue, mkMintNftTrx, submitTrx, toDatum, toRedeemer)
import Coop.Types (
  AuthDeployment (AuthDeployment, ad'authMp, ad'authorityAc, ad'certMp, ad'certV),
  AuthMpParams (AuthMpParams),
  AuthMpRedeemer (AuthMpBurn, AuthMpMint),
  AuthParams (AuthParams, ap'authTokenCs, ap'certTokenCs),
  CertDatum (CertDatum),
  CertMpParams (CertMpParams),
  CertMpRedeemer (CertMpBurn, CertMpMint),
  CoopDeployment (CoopDeployment, cd'auth, cd'fsMp, cd'fsV),
  CoopPlutus (cp'fsV, cp'mkAuthMp, cp'mkCertMp, cp'mkFsMp, cp'mkNftMp),
  FsDatum,
  FsMpParams (FsMpParams),
  FsMpRedeemer (FsMpMint),
  cp'certV,
 )
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import Ledger (POSIXTimeRange, PaymentPubKeyHash, applyArguments, getCardanoTxId, scriptCurrencySymbol, validatorHash)
import Ledger.Tx (ChainIndexTxOut, ciTxOutValue)
import Plutus.Contract (Contract, submitTxConstraintsWith, throwError)
import Plutus.Contract.Constraints (mustBeSignedBy, mustMintValueWithRedeemer, mustPayToOtherScript, mustPayToPubKey, mustSpendPubKeyOutput, mustSpendScriptOutput, otherData, ownPaymentPubKeyHash, plutusV1MintingPolicy, plutusV1OtherScript, unspentOutputs)
import Plutus.Contract.Logging (logInfo)
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import Plutus.V1.Ledger.Api (
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
import Plutus.V1.Ledger.Value (AssetClass, assetClass)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.Prelude (Group (inv))
import Test.Plutip.Internal.BotPlutusInterface.Setup ()
import Test.Plutip.Internal.LocalCluster ()
import Text.Printf (printf)

deployCoop :: CoopPlutus -> PaymentPubKeyHash -> PaymentPubKeyHash -> Integer -> Contract w s Text CoopDeployment
deployCoop coopPlutus self aaWallet atLeastAaQ = do
  let logI m = logInfo @String ("deployCoop: " <> m)
  logI "Starting"

  outs <- findOutsAt' @Void self (\_ _ -> True)
  (aaOut, coopOut) <- case Map.toList outs of
    o1 : o2 : _ -> return (o1, o2)
    _ -> throwError "deployCoop: Not enough outputs found to use for making NFTs"

  let mkNftMp = cp'mkNftMp coopPlutus
      (mintAaTrx, aaAc) = mkMintNftTrx self aaWallet aaOut mkNftMp atLeastAaQ
      (mintCoopTrx, coopAc) = mkMintNftTrx self self coopOut mkNftMp 1

  submitTrx @Void (mintAaTrx <> mintCoopTrx)
  logI $ "Created $COOP instance token: " <> show coopAc
  logI $ "Created $AA authentication token: " <> show aaAc

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

mintCertRedeemers :: CoopPlutus -> PaymentPubKeyHash -> PaymentPubKeyHash -> Integer -> Contract w s Text AssetClass
mintCertRedeemers coopPlutus self certRedeemerWallet q = do
  let logI m = logInfo @String ("mintCertR: " <> m)
  logI "Starting"

  outs <- findOutsAt' @Void self (\_ _ -> True)
  out <- case reverse . Map.toList $ outs of -- FIXME: If I don't shuffle this I get InsuficientCollateral
    o1 : _ -> return o1
    _ -> throwError "mintCertR: Not enough outputs found to use for making NFTs"

  let mkNftMp = cp'mkNftMp coopPlutus
      (mintCertRTrx, certRAc) = mkMintNftTrx self certRedeemerWallet out mkNftMp q

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
      hashedAaOrefs = hashTxOutRefs aaOrefs
      certTn = TokenName hashedAaOrefs
      certId = LedgerBytes hashedAaOrefs
      certCs = scriptCurrencySymbol certMp
      certVal = Value.singleton certCs certTn 1
      certDatum = toDatum $ CertDatum certId validityInterval redeemerAc
      lookups =
        mconcat
          [ plutusV1MintingPolicy certMp
          , plutusV1OtherScript certV
          , otherData certDatum
          , ownPaymentPubKeyHash self
          , unspentOutputs aaOuts
          ]
      constraints =
        mustMintValueWithRedeemer (Redeemer . toBuiltinData $ CertMpMint) certVal
          <> mustPayToOtherScript certVAddr certDatum (certVal <> minUtxoAdaValue)
          <> mconcat (mustSpendPubKeyOutput <$> aaOrefs)
   in (Trx lookups constraints, assetClass certCs certTn)

burnCerts :: CoopDeployment -> PaymentPubKeyHash -> Map TxOutRef ChainIndexTxOut -> Map TxOutRef ChainIndexTxOut -> Contract w s Text TxId
burnCerts coopDeployment self certOuts redeemerOuts = do
  let logI m = logInfo @String ("burnCert: " <> m)
  logI "Starting"
  let certMp = (ad'certMp . cd'auth) coopDeployment
      certV = (ad'certV . cd'auth) coopDeployment
      redeemerOrefs = Map.keys redeemerOuts
      certOrefs = Map.keys certOuts
      certCs = scriptCurrencySymbol certMp
      certVal = foldMap (\out -> inv $ currencyValue (out ^. ciTxOutValue) certCs) (toList certOuts)

  let lookups =
        plutusV1MintingPolicy certMp
          <> plutusV1OtherScript certV
          <> ownPaymentPubKeyHash self
          <> unspentOutputs redeemerOuts
          <> unspentOutputs certOuts

      tx =
        mustMintValueWithRedeemer (Redeemer . toBuiltinData $ CertMpBurn) certVal
          <> mconcat (mustSpendPubKeyOutput <$> redeemerOrefs)
          <> mconcat ((\oref -> mustSpendScriptOutput oref (toRedeemer ())) <$> certOrefs)

  tx <- submitTxConstraintsWith @Void lookups tx
  logI "Finished"
  return (getCardanoTxId tx)

mkMintAuthTrx :: CoopDeployment -> PaymentPubKeyHash -> [PaymentPubKeyHash] -> Integer -> Map TxOutRef ChainIndexTxOut -> (Trx i o a, AssetClass)
mkMintAuthTrx coopDeployment self authWallets authQEach aaOuts =
  let authMp = (ad'authMp . cd'auth) coopDeployment
      aaOrefs = Map.keys aaOuts
      hashedAaOrefs = hashTxOutRefs aaOrefs
      authTn = TokenName hashedAaOrefs
      authCs = scriptCurrencySymbol authMp
      authValEach = Value.singleton authCs authTn authQEach
      authValTotal = Value.singleton authCs authTn (authQEach * (fromIntegral . length $ authWallets))
      lookups =
        plutusV1MintingPolicy authMp
          <> ownPaymentPubKeyHash self
          <> unspentOutputs aaOuts
      constraints =
        mustMintValueWithRedeemer (Redeemer . toBuiltinData $ AuthMpMint) authValTotal
          <> mconcat ((`mustPayToPubKey` (authValEach <> minUtxoAdaValue)) <$> authWallets)
          <> mconcat (mustSpendPubKeyOutput <$> aaOrefs)
   in (Trx lookups constraints, assetClass authCs authTn)

mkBurnAuthsTrx :: CoopDeployment -> PaymentPubKeyHash -> Map TxOutRef ChainIndexTxOut -> Trx i o a
mkBurnAuthsTrx coopDeployment self authOuts = do
  let authMp = (ad'authMp . cd'auth) coopDeployment
      authCs = scriptCurrencySymbol authMp
      authOrefs = Map.keys authOuts
      authVal = foldMap (\out -> inv $ currencyValue (out ^. ciTxOutValue) authCs) (toList authOuts)

      lookups =
        plutusV1MintingPolicy authMp
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
mkMintFsTrx :: CoopDeployment -> PaymentPubKeyHash -> FsDatum -> (TxOutRef, ChainIndexTxOut) -> ((TxOutRef, ChainIndexTxOut), CertDatum) -> PaymentPubKeyHash -> (Trx i o a, AssetClass)
mkMintFsTrx coopDeployment self fsDatum authOut (certOut, certDatum) submitterPpkh = do
  let fsMp = cd'fsMp coopDeployment
      fsV = cd'fsV coopDeployment
      fsVAddr = validatorHash fsV
      fsCs = scriptCurrencySymbol fsMp
      fsTn = TokenName . hashTxOutRefs $ [fst authOut]
      fsVal = Value.singleton fsCs fsTn 1
      certV = ad'certV . cd'auth $ coopDeployment
      lookups =
        plutusV1MintingPolicy fsMp
          <> plutusV1OtherScript fsV
          <> plutusV1OtherScript certV
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
      mintFsTrx =
        mkBurnAuthsTrx coopDeployment self (Map.fromList [authOut])
          <> Trx lookups constraints
   in -- <> mkBurnCertsTrx
      (mintFsTrx, assetClass fsCs fsTn)
