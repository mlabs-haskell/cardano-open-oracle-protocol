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
  mintAuthAndCert,
  getState,
  runMintFsTx,
  runRedistributeAuthsTrx,
) where

import BotPlutusInterface.Constraints (mustValidateInFixed)
import Cardano.Proto.Aux (
  ProtoCardano (fromCardano, toCardano),
 )
import Control.Lens ((&), (.~), (^.))
import Control.Monad (guard)
import Coop.Pab.Aux (Trx (Trx), currencyValue, datumFromTxOut, datumFromTxOutOrFail, deplAuthCs, deplAuthMp, deplCertCs, deplCertVAddress, deplFsCs, deplFsVAddress, deplFsVHash, findOutsAt, findOutsAt', findOutsAtHolding', findOutsAtHoldingCurrency, findOutsAtHoldingCurrency', hasCurrency, hashTxInputs, interval', minUtxoAdaValue, mkMintOneShotTrx, submitTrx, toDatum, toRedeemer)
import Coop.Types (AuthBatchId, AuthDeployment (AuthDeployment, ad'authMp, ad'authorityAc, ad'certMp, ad'certV), AuthMpParams (AuthMpParams), AuthMpRedeemer (AuthMpBurn, AuthMpMint), AuthParams (AuthParams, ap'authTokenCs, ap'certTokenCs), CertDatum (CertDatum, cert'id, cert'validity), CertMpParams (CertMpParams), CertMpRedeemer (CertMpBurn, CertMpMint), CoopDeployment (CoopDeployment, cd'auth, cd'fsMp, cd'fsV), CoopPlutus (cp'fsV, cp'mkAuthMp, cp'mkCertMp, cp'mkFsMp, cp'mkOneShotMp), CoopState (CoopState), FactStatementId, FsDatum (FsDatum, fd'fsId), FsMpParams (FsMpParams), FsMpRedeemer (FsMpMint), cp'certV)
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.List (nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Traversable (for)
import Data.Void (Void)
import Ledger (Language (PlutusV2), PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash), TxOutRef, Versioned (Versioned), after, applyArguments, ciTxOutValue, interval)
import Ledger.Tx (ChainIndexTxOut)
import Plutus.Contract (Contract, currentNodeClientTimeRange, ownFirstPaymentPubKeyHash, throwError)
import Plutus.Contract.Constraints (mintingPolicy, mustBeSignedBy, mustMintValueWithRedeemer, mustPayToOtherScriptWithInlineDatum, mustPayToPubKey, mustReferenceOutput, mustSpendPubKeyOutput, mustSpendScriptOutput, otherData, otherScript, ownPaymentPubKeyHash, plutusV2MintingPolicy, plutusV2OtherScript, unspentOutputs)
import Plutus.Contract.Logging (logInfo)
import Plutus.Script.Utils.V2.Address (mkValidatorAddress)
import Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol, validatorHash)
import Plutus.V1.Ledger.Value (
  AssetClass,
  assetClass,
  assetClassValue,
 )
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api (
  Extended (Finite, PosInf),
  Interval (ivTo),
  LedgerBytes (LedgerBytes, getLedgerBytes),
  MintingPolicy (MintingPolicy),
  POSIXTime,
  POSIXTimeRange,
  TokenName (TokenName),
  UpperBound (UpperBound),
  Validator (Validator),
  Value,
  toBuiltin,
  toData,
 )
import PlutusTx (toBuiltinData)
import PlutusTx.Prelude (Group (inv))
import Proto.TxBuilderService (CreateMintFsTxReq, MintFsSuccess'FsIdAndTxOutRef)
import Proto.TxBuilderService_Fields (factStatementId, factStatementUtxo, factStatements, fs, fsId, gcAfter, submitter)
import Safe.Foldable (maximumMay)
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
    (atLeastAaQ <= aaQToMint)

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
    authParamsFromDeployment ad =
      AuthParams
        { ap'authTokenCs = scriptCurrencySymbol (ad'authMp ad)
        , ap'certTokenCs = scriptCurrencySymbol (ad'certMp ad)
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
  logI $ printf "Created $CERT-RDMR redeemer token for redeeming obsolete $CERT outputs @CertV: %s" (show certRAc)
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
      certCs = deplCertCs coopDeployment
      certVal = Value.singleton certCs certTn 1
      certDatum = toDatum $ CertDatum (LedgerBytes certId) validityInterval redeemerAc
      lookups =
        mconcat
          [ mintingPolicy (Versioned certMp PlutusV2)
          , otherScript (Versioned certV PlutusV2)
          , otherData certDatum
          , ownPaymentPubKeyHash self
          , unspentOutputs aaOuts
          ]
      constraints =
        mustMintValueWithRedeemer (toRedeemer CertMpMint) certVal
          -- FIXME: Figure out why mustPayToOtherScriptWithDatumInline fails
          <> mustPayToOtherScriptWithInlineDatum certVAddr certDatum (certVal <> minUtxoAdaValue)
          <> mconcat (mustSpendPubKeyOutput <$> aaOrefs)
   in (Trx lookups constraints [], assetClass certCs certTn)

burnCerts :: CoopDeployment -> AssetClass -> Contract w s Text ()
burnCerts coopDeployment certRdmrAc = do
  let logI m = logInfo @String ("burnCerts: " <> m)
  logI "Starting"
  let certCs = deplCertCs coopDeployment
      certVAddr = deplCertVAddress coopDeployment
  self <- ownFirstPaymentPubKeyHash
  certRdmrOuts <- findOutsAtHolding' self certRdmrAc
  certOuts <- findOutsAtHoldingCurrency certVAddr certCs
  logI $ printf "Found %d $CERT ouputs at @CertV" (length certOuts)

  bool
    (throwError "burnCerts: There should be some $CERT inputs")
    (pure ())
    $ not (null certOuts)

  (now, _) <- currentNodeClientTimeRange
  let certMp = (ad'certMp . cd'auth) coopDeployment
      certV = (ad'certV . cd'auth) coopDeployment
      certRdmdrOrefs = Map.keys certRdmrOuts
      certOrefs = Map.keys certOuts
      certVal = foldMap (\out -> inv $ currencyValue (out ^. ciTxOutValue) certCs) (toList certOuts)
      timeRange = interval' (Finite now) PosInf

  let lookups =
        mintingPolicy (Versioned certMp PlutusV2)
          <> otherScript (Versioned certV PlutusV2)
          <> ownPaymentPubKeyHash self
          <> unspentOutputs certRdmrOuts
          <> unspentOutputs certOuts

      constraints =
        mustMintValueWithRedeemer (toRedeemer CertMpBurn) certVal
          <> mconcat (mustSpendPubKeyOutput <$> certRdmdrOrefs)
          <> mconcat ((\oref -> mustSpendScriptOutput oref (toRedeemer ())) <$> certOrefs)

      bpiConstraints = mustValidateInFixed timeRange

  submitTrx @Void (Trx lookups constraints bpiConstraints)
  logI "Finished"

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
   in (Trx lookups constraints [], assetClass authCs authTn)

runRedistributeAuthsTrx :: CoopDeployment -> PaymentPubKeyHash -> Int -> Contract w s Text ()
runRedistributeAuthsTrx coopDeployment self howMany = do
  let authCs = deplAuthCs coopDeployment
  authOuts <- findOutsAtHoldingCurrency' self authCs
  logInfo @String (show authOuts)
  let authAcs =
        [ assetClass cs tn
        | out <- toList authOuts
        , (cs, tn, q) <- Value.flattenValue (out ^. ciTxOutValue)
        , q >= toInteger howMany
        ]
  case authAcs of
    [] -> throwError "Must have at least $AUTH x howMany to redistribute"
    authAc : _ -> do
      let lookups = ownPaymentPubKeyHash self <> unspentOutputs authOuts
          constraints =
            mconcat (mustSpendPubKeyOutput <$> Map.keys authOuts)
              <> mconcat (replicate howMany (mustPayToPubKey self (assetClassValue authAc 1)))

      submitTrx @Void (Trx lookups constraints [])
      authOuts' <- findOutsAtHoldingCurrency' self authCs
      logInfo @String (show authOuts')

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
  (_, now) <- currentNodeClientTimeRange
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
   in Trx lookups constraints []

burnAuths :: CoopDeployment -> PaymentPubKeyHash -> Map TxOutRef ChainIndexTxOut -> Contract w s Text ()
burnAuths coopDeployment self authOuts = do
  let logI m = logInfo @String ("burnAuths: " <> m)
  logI "Starting"
  let trx = mkBurnAuthsTrx coopDeployment self authOuts
  submitTrx @Void trx
  logI "Finished"

getState :: CoopDeployment -> Contract w s Text CoopState
getState coopDeployment = do
  let logI m = logInfo @String ("getState: " <> m)
  logI "Starting"

  let certVAddr = mkValidatorAddress . ad'certV . cd'auth $ coopDeployment
      certCs = scriptCurrencySymbol . ad'certMp . cd'auth $ coopDeployment
      fsVAddr = mkValidatorAddress . cd'fsV $ coopDeployment
      fsCs = scriptCurrencySymbol . cd'fsMp $ coopDeployment

  certOuts <- findOutsAtHoldingCurrency certVAddr certCs
  fsOuts <- findOutsAtHoldingCurrency fsVAddr fsCs

  certDatums <-
    traverse
      ( \(_, ciOut) -> do
          mayDat <- datumFromTxOut @CertDatum ciOut
          maybe (throwError "Can't parse CertDatum") return mayDat
      )
      (Map.toList certOuts)
  fsDatums <-
    traverse
      ( \(_, ciOut) -> do
          mayDat <- datumFromTxOut @FsDatum ciOut
          maybe (throwError "Can't parse FsDatum") return mayDat
      )
      (Map.toList fsOuts)

  logI "Finished"
  return (CoopState certDatums fsDatums)

-- | Queries
findOutsAtCertV :: CoopDeployment -> (Value -> CertDatum -> Bool) -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findOutsAtCertV coopDeployment p = do
  let logI m = logInfo @String ("findOutsAtCertV: " <> m)

  logI "Starting"

  let certVAddr = (mkValidatorAddress . ad'certV . cd'auth) coopDeployment
  findOutsAt @CertDatum
    certVAddr
    (maybe False . p)

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

data CoopOnchainState = CoopOnchainState
  { _authBatches :: Map AuthBatchId CoopAuthBatch
  , _published :: Map FactStatementId TxOutRef
  }

data CoopAuthBatch = CoopAuthBatch
  { auth'certDatum :: CertDatum
  , auth'certOut :: (TxOutRef, ChainIndexTxOut)
  , auth'authOutsByWallet :: Map PaymentPubKeyHash (Map TxOutRef ChainIndexTxOut)
  , auth'isExpired :: Bool
  }

findPublished :: CoopDeployment -> Contract w s Text (Map FactStatementId TxOutRef)
findPublished coopDeployment = do
  let fsVAddr = deplFsVAddress coopDeployment
      fsCs = deplFsCs coopDeployment
  fsOuts <- Map.toList <$> findOutsAtHoldingCurrency fsVAddr fsCs
  fsOrefsByFsId <-
    for
      fsOuts
      ( \(fsOref, fsOut) -> do
          fsDatum <- datumFromTxOutOrFail fsOut "@FsV UTxOs must have a FsDatum"
          return (fd'fsId fsDatum, fsOref)
      )
  return $ Map.fromList fsOrefsByFsId

findAuthentication :: CoopDeployment -> [PaymentPubKeyHash] -> Contract w s Text (Map LedgerBytes CoopAuthBatch)
findAuthentication coopDeployment authenticators = do
  let certCs = deplCertCs coopDeployment
      authCs = deplAuthCs coopDeployment
      certVAddr = deplCertVAddress coopDeployment
  (_, now) <- currentNodeClientTimeRange
  certOuts <- Map.toList <$> findOutsAtHoldingCurrency certVAddr certCs
  authBatches <-
    for
      certOuts
      ( \(certOref, certOut) -> do
          certDatum <- datumFromTxOutOrFail certOut "@CertV UTxOs must have a CertDatum"
          let certId = cert'id certDatum
              authAc = assetClass authCs (TokenName . getLedgerBytes $ certId)
              isExpired = after now (cert'validity certDatum)
          authOutsByWallet <-
            for
              authenticators
              ( \auth -> do
                  authOuts <- findOutsAtHolding' auth authAc
                  return (auth, authOuts)
              )
          return (certId, CoopAuthBatch certDatum (certOref, certOut) (Map.fromList authOutsByWallet) isExpired)
      )

  return (Map.fromList authBatches)

findOnchainState :: CoopDeployment -> [PaymentPubKeyHash] -> Contract w s Text CoopOnchainState
findOnchainState coopDeployment authenticators =
  CoopOnchainState
    <$> findAuthentication coopDeployment authenticators
    <*> findPublished coopDeployment

toCardanoC :: ProtoCardano (Either Text) proto cardano => proto -> Contract w s Text cardano
toCardanoC proto = let x = toCardano @(Either Text) proto in either throwError return x

mkPublishingSpec :: CoopDeployment -> [PaymentPubKeyHash] -> CreateMintFsTxReq -> Contract w s Text (PublishingSpec, [MintFsSuccess'FsIdAndTxOutRef])
mkPublishingSpec coopDeployment authenticators req = do
  CoopOnchainState authBatches published <- findOnchainState coopDeployment authenticators
  submitterPPkh <- PaymentPubKeyHash <$> toCardanoC (req ^. submitter)
  let alreadyPublished' =
        [ defMessage
          & factStatementId .~ fsInfo ^. fsId
          & factStatementUtxo .~ fsOref'
        | fsInfo <- req ^. factStatements
        , fsOref <- maybe [] return $ Map.lookup (LedgerBytes . toBuiltin $ fsInfo ^. fsId) published
        , fsOref' <- fromCardano fsOref
        ]
      fsInfosToPublish =
        [ fsInfo
        | fsInfo <- req ^. factStatements
        , not $ Map.member (LedgerBytes . toBuiltin $ fsInfo ^. fsId) published
        ]
      authInfos = take (length fsInfosToPublish) $ flattenAuthBatches authBatches
  txValidUntil <-
    maybe
      (throwError "Must have sufficient $AUTH inputs")
      return
      (maximumMay $ (\(_, _, _, UpperBound ext _) -> ext) <$> authInfos)
  fsDatums <-
    for
      fsInfosToPublish
      ( \fsInfo -> do
          gc <- toCardanoC $ fsInfo ^. gcAfter
          return $
            FsDatum
              (toBuiltinData $ fsInfo ^. fs)
              (LedgerBytes . toBuiltin $ fsInfo ^. fsId)
              gc
              (unPaymentPubKeyHash submitterPPkh)
      )
  let fsInfosWithAuth =
        zipWith
          ( \(authBatch, authOut, authWallet, _) fsDatum -> FactStatementSpec fsDatum authOut authBatch authWallet
          )
          authInfos
          fsDatums
  if length fsInfosWithAuth /= length fsInfosToPublish
    then throwError "Must have enough $AUTH outputs to authenticate each Fact Statement"
    else return (PublishingSpec submitterPPkh fsInfosWithAuth txValidUntil, alreadyPublished')
  where
    flattenAuthBatches :: Map LedgerBytes CoopAuthBatch -> [(CoopAuthBatch, (TxOutRef, ChainIndexTxOut), PaymentPubKeyHash, UpperBound POSIXTime)]
    flattenAuthBatches authBatches = do
      batch <- toList authBatches
      guard $ not (auth'isExpired batch)
      (authWallet, authOuts) <- Map.toList $ auth'authOutsByWallet batch
      authOut <- Map.toList authOuts
      return (batch, authOut, authWallet, ivTo . cert'validity . auth'certDatum $ batch)

runMintFsTx :: CoopDeployment -> [PaymentPubKeyHash] -> (Value, PaymentPubKeyHash) -> CreateMintFsTxReq -> Contract w s Text [MintFsSuccess'FsIdAndTxOutRef]
runMintFsTx coopDeployment authenticators feeSpec req = do
  (publishingSpec, alreadyPublished') <- mkPublishingSpec coopDeployment authenticators req
  (_, now) <- currentNodeClientTimeRange
  let mintFsTrx =
        mkMintFsTrx'
          coopDeployment
          now
          publishingSpec
          feeSpec
  submitTrx @Void mintFsTrx
  -- TODO: Add the transaction body in cbor
  return alreadyPublished'

data PublishingSpec = PublishingSpec
  { ps'submitter :: PaymentPubKeyHash
  , ps'factStatementSpecs :: [FactStatementSpec]
  , ps'validUntil :: Extended POSIXTime
  }

data FactStatementSpec = FactStatementSpec
  { fss'fsDatum :: FsDatum
  , fss'authOut :: (TxOutRef, ChainIndexTxOut)
  , fss'authBatch :: CoopAuthBatch
  , fss'authWallet :: PaymentPubKeyHash
  }

mkMintFsTrx' ::
  CoopDeployment ->
  POSIXTime ->
  PublishingSpec ->
  (Value, PaymentPubKeyHash) ->
  Trx i o a
mkMintFsTrx' coopDeployment now publishingSpec (feeVal, feeCollectorPpkh) = do
  let fsMp = cd'fsMp coopDeployment
      fsV = cd'fsV coopDeployment
      fsVHash = deplFsVHash coopDeployment
      fsCs = deplFsCs coopDeployment
      authCs = deplAuthCs coopDeployment
      authMp = deplAuthMp coopDeployment

      mkFsTn authOut = TokenName . hashTxInputs $ Map.fromList [authOut]
      totalFsValToMint =
        mconcat
          [ Value.singleton fsCs (mkFsTn (fss'authOut fsSpec)) 1 | fsSpec <- ps'factStatementSpecs publishingSpec
          ]

      authValueFromBatch authBatch = assetClassValue (assetClass authCs (TokenName . getLedgerBytes . cert'id . auth'certDatum $ authBatch))
      lookups =
        -- TODO: Add ref inputs @CertV
        -- Minting $FS tokens
        plutusV2MintingPolicy fsMp
          -- Paying to @FsV
          <> plutusV2OtherScript fsV
          -- FsDatums
          <> mconcat
            [ otherData (toDatum (fss'fsDatum fsSpec))
            | fsSpec <- ps'factStatementSpecs publishingSpec
            ]
          -- Self is the Submitter
          <> ownPaymentPubKeyHash (ps'submitter publishingSpec)
          -- CertDatums
          <> mconcat
            ( [ otherData . toDatum . auth'certDatum . fss'authBatch $ fsSpec
              | fsSpec <- ps'factStatementSpecs publishingSpec
              ]
            )
          -- Burning $AUTH tokens
          <> plutusV2MintingPolicy authMp
          -- \$AUTH UTxOs
          <> unspentOutputs (Map.fromList [fss'authOut fsSpec | fsSpec <- ps'factStatementSpecs publishingSpec])

      constraints =
        -- Mint all the $FS tokens associated with Fact Statements
        mustMintValueWithRedeemer (toRedeemer FsMpMint) totalFsValToMint
          -- Pay all the Fact Statements to @FsV
          <> mconcat
            [ mustPayToOtherScriptWithInlineDatum
              fsVHash
              (toDatum (fss'fsDatum fsSpec))
              (Value.singleton fsCs (mkFsTn (fss'authOut fsSpec)) 1 <> minUtxoAdaValue)
            | fsSpec <- ps'factStatementSpecs publishingSpec
            ]
          -- Spend all Authenticator with $AUTH tokens
          <> mconcat
            [ mustSpendPubKeyOutput (fst (fss'authOut fsSpec))
            | fsSpec <- ps'factStatementSpecs publishingSpec
            ]
          -- Must burn used $AUTH tokens
          <> mconcat
            [ mustMintValueWithRedeemer
              (toRedeemer AuthMpBurn)
              (authValueFromBatch (fss'authBatch fsSpec) (-1))
            | fsSpec <- ps'factStatementSpecs publishingSpec
            ]
          -- Pay back all unused value to Authenticator wallets
          <> mconcat
            [ mustPayToPubKey
              (fss'authWallet fsSpec)
              (snd (fss'authOut fsSpec) ^. ciTxOutValue <> authValueFromBatch (fss'authBatch fsSpec) (-1))
            | fsSpec <- ps'factStatementSpecs publishingSpec
            ]
          -- Reference all necessary unique Certificate outputs
          <> mconcat
            ( mustReferenceOutput
                <$> nub
                  [ fst . auth'certOut . fss'authBatch $ fsSpec
                  | fsSpec <- ps'factStatementSpecs publishingSpec
                  ]
            )
          -- Sign with all used Authenticators
          <> mconcat
            [ mustBeSignedBy (fss'authWallet fsSpec)
            | fsSpec <- ps'factStatementSpecs publishingSpec
            ]
          -- Pay the $FEE to $FEE Collector
          <> mustPayToPubKey feeCollectorPpkh feeVal
          -- Sign with Submitter
          <> mustBeSignedBy (ps'submitter publishingSpec)

      -- Validation range needed to validate certificates
      bpiConstraints = mustValidateInFixed (interval' (Finite now) (ps'validUntil publishingSpec))
   in Trx lookups constraints bpiConstraints
