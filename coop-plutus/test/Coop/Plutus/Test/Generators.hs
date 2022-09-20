module Coop.Plutus.Test.Generators (mkScriptContext, mkTxInfo, genCertRdmrAc, distribute, genCorruptCertMpMintingCtx, genAaInputs, genCorrectCertMpMintingCtx, genCorrectAuthMpMintingCtx, genCorruptAuthMpMintingCtx, genCorrectCertMpBurningCtx, genCorruptCertMpBurningCtx, normalizeValue, genCorrectAuthMpBurningCtx, genCorruptAuthMpBurningCtx, genCorrectCertVSpendingCtx) where

import Test.QuickCheck (Arbitrary (arbitrary), Gen, choose, chooseAny, chooseEnum, chooseInt, chooseInteger, vectorOf)

import Control.Monad (foldM, replicateM)
import Coop.Plutus.Aux (hashTxInputs)
import Data.Foldable (Foldable (fold, foldl'))
import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (for)
import PlutusLedgerApi.V1.Address (pubKeyHashAddress, scriptHashAddress)
import PlutusLedgerApi.V1.Value (AssetClass, CurrencySymbol (CurrencySymbol), TokenName (TokenName), assetClass, assetClassValue, assetClassValueOf)
import PlutusLedgerApi.V2 (Address, BuiltinByteString, Datum (Datum), LedgerBytes (LedgerBytes), OutputDatum (NoOutputDatum, OutputDatum), POSIXTime (POSIXTime), PubKeyHash (PubKeyHash), ScriptContext (ScriptContext, scriptContextTxInfo), ScriptPurpose (Minting, Spending), ToData, TxId (TxId), TxInInfo (TxInInfo, txInInfoOutRef), TxInfo (TxInfo, txInfoDCert, txInfoData, txInfoFee, txInfoId, txInfoInputs, txInfoMint, txInfoOutputs, txInfoRedeemers, txInfoReferenceInputs, txInfoSignatories, txInfoValidRange, txInfoWdrl), TxOut (TxOut, txOutAddress, txOutDatum, txOutValue), TxOutRef (TxOutRef), ValidatorHash (ValidatorHash), Value (Value, getValue), always, toBuiltin, toBuiltinData)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins.Class (stringToBuiltinByteString)

import Coop.Types (AuthMpParams (amp'authAuthorityAc, amp'requiredAtLeastAaQ), CertDatum (CertDatum), CertMpParams (cmp'authAuthorityAc, cmp'certVAddress, cmp'requiredAtLeastAaQ))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Word (Word8)
import PlutusLedgerApi.V1.Interval (interval)
import PlutusLedgerApi.V2 qualified as Value
import PlutusTx.Prelude (Group (inv))

mkScriptContext :: ScriptPurpose -> [TxInInfo] -> [TxInInfo] -> Value -> [TxOut] -> [PubKeyHash] -> ScriptContext
mkScriptContext purpose ins refs mints outs sigs =
  ScriptContext (mkTxInfo ins refs mints outs sigs) purpose

mkTxInfo :: [TxInInfo] -> [TxInInfo] -> Value -> [TxOut] -> [PubKeyHash] -> TxInfo
mkTxInfo ins refs mints outs sigs =
  TxInfo
    { txInfoFee = mempty
    , txInfoDCert = mempty
    , txInfoWdrl = AssocMap.empty
    , txInfoValidRange = always
    , txInfoData = AssocMap.empty
    , txInfoId = ""
    , txInfoRedeemers = AssocMap.empty
    , txInfoInputs = sortOn (\(TxInInfo i _) -> i) ins
    , txInfoReferenceInputs = sortOn (\(TxInInfo i _) -> i) refs
    , txInfoMint = normalizeValue mints
    , txInfoOutputs = outs
    , txInfoSignatories = sigs
    }

genAaInputs :: AssetClass -> Integer -> Gen [TxInInfo]
genAaInputs aaAc aaQ = do
  aaTokens <- choose (aaQ, aaQ + 10) >>= \n -> return $ replicate (fromInteger n) . assetClassValue aaAc $ 1
  aaWallets <- choose (1, length aaTokens) >>= \n -> for [1 .. n] $ \ix -> return . pubKeyHashAddress . PubKeyHash . stringToBuiltinByteString $ "pubkeyhash " <> show ix
  aaOrefs <- choose (1, length aaTokens) >>= \n -> for [1 .. n] $ \ix -> return $ TxOutRef (TxId $ "transaction " <> (stringToBuiltinByteString . show $ ix)) (toInteger ix)
  aaOrefsWithTokens <- fmap fold <$> distribute aaTokens (Set.fromList aaOrefs)
  aaWalletsWithOrefs <- distribute aaOrefs (Set.fromList aaWallets)
  let aaOutsByAddr =
        ( \orefs -> do
            oref <- orefs
            maybe
              []
              (\tokens -> return (oref, tokens))
              $ Map.lookup oref aaOrefsWithTokens
        )
          <$> aaWalletsWithOrefs

  for
    [(addr, oref, val) | (addr, outs) <- Map.toList aaOutsByAddr, (oref, val) <- outs, assetClassValueOf val aaAc > 0]
    $ \(addr, oref, val) -> return $ TxInInfo oref (TxOut addr val NoOutputDatum Nothing)

toOutputDatum :: ToData a => a -> OutputDatum
toOutputDatum = OutputDatum . Datum . toBuiltinData

genCertRdmrInputs :: AssetClass -> Gen [TxInInfo]
genCertRdmrInputs certRdmrAc = do
  nCertRdmrInputs <- chooseInt (1, 10)
  certRdmrAddrs <- replicateM nCertRdmrInputs genAddress
  return
    [ TxInInfo
      (TxOutRef (TxId "$CERT-RDMR input") 0)
      ( TxOut
          addr
          (assetClassValue certRdmrAc 1)
          NoOutputDatum
          Nothing
      )
    | addr <- certRdmrAddrs
    ]

genCertInputs :: Address -> CurrencySymbol -> AssetClass -> Integer -> Gen [TxInInfo]
genCertInputs certVAddr certCs certRdmrAc validUntil = do
  nCertInputs <- chooseInt (1, 10)
  certIds <- replicateM nCertInputs genAuthenticatonId
  certValidities <-
    replicateM
      nCertInputs
      ( do
          lowerBound <- chooseInteger (0, validUntil)
          upperBound <- chooseInteger (lowerBound, validUntil)
          return $ interval (POSIXTime lowerBound) (POSIXTime upperBound)
      )

  let certInputs =
        ( \(certId, certValidity) ->
            TxInInfo
              (TxOutRef (TxId certId) 0)
              ( TxOut
                  certVAddr
                  (Value.singleton certCs (TokenName certId) 1)
                  (toOutputDatum $ CertDatum (LedgerBytes certId) certValidity certRdmrAc)
                  Nothing
              )
        )
          <$> zip certIds certValidities
  return certInputs

genAuthInputs :: CurrencySymbol -> Gen [TxInInfo]
genAuthInputs authCs = do
  nAuthInputs <- chooseInt (1, 10)
  authIds <- replicateM nAuthInputs genAuthenticatonId
  authQs <- replicateM nAuthInputs (chooseInteger (1, 10))
  authWallets <- replicateM 5 genAddress
  authWalletsWithIdsAndQs <- distribute (zip authIds authQs) $ Set.fromList authWallets

  let authInputs =
        ( \(authWallet, authId, authQ) ->
            TxInInfo
              (TxOutRef (TxId authId) 0)
              ( TxOut
                  authWallet
                  (Value.singleton authCs (TokenName authId) authQ)
                  NoOutputDatum
                  Nothing
              )
        )
          <$> [(authWallet, authId, authQ) | (authWallet, idsAndQs) <- Map.toList authWalletsWithIdsAndQs, (authId, authQ) <- idsAndQs]
  return authInputs

genCertRdmrAc :: Gen AssetClass
genCertRdmrAc = do
  certRdmrCs <- genCurrencySymbol
  return $ assetClass certRdmrCs (TokenName "$CERT-RDMR TN")

genCorrectCertMpMintingCtx :: CertMpParams -> CurrencySymbol -> Gen ScriptContext
genCorrectCertMpMintingCtx certMpParams certCs = do
  let aaAc = cmp'authAuthorityAc certMpParams
      aaQ = cmp'requiredAtLeastAaQ certMpParams
      certVAddr = cmp'certVAddress certMpParams
  aaIns <- genAaInputs aaAc aaQ
  certRdmrAc <- genCertRdmrAc
  let certId = toBuiltin . hashTxInputs $ aaIns
      certTn = TokenName certId
      certToken = assetClassValue (assetClass certCs certTn) 1
      certDatum = CertDatum (LedgerBytes certId) (interval 0 100) certRdmrAc
      certOut = TxOut certVAddr certToken (OutputDatum . Datum . toBuiltinData $ certDatum) Nothing
  return $
    mkScriptContext (Minting certCs) aaIns [] certToken [certOut] []

genCorruptCertMpMintingCtx :: CertMpParams -> CurrencySymbol -> Gen ScriptContext
genCorruptCertMpMintingCtx certMpParams certCs = do
  let certVAddr = cmp'certVAddress certMpParams

  ctx <- genCorrectCertMpMintingCtx certMpParams certCs

  -- Randomly pick a corruption
  ((mintAndPayOtherTokenName, removeOutputDatum, sendToOtherAddress) :: (Bool, Bool, Bool)) <- arbitrary

  let corrupt =
        mkCorrupt
          [ (mintAndPayOtherTokenName, doMintAndPayOtherTokenName certCs)
          , (removeOutputDatum, doRemoveOutputDatum)
          , (sendToOtherAddress, doSendToOtherAddress certVAddr)
          ]

  -- If we didn't manage to corrupt anything, do it again
  let corruptedCtx = corrupt ctx
  if corruptedCtx == ctx
    then genCorruptCertMpMintingCtx certMpParams certCs
    else return corruptedCtx

genCorrectCertMpBurningCtx :: CertMpParams -> CurrencySymbol -> AssetClass -> Gen ScriptContext
genCorrectCertMpBurningCtx certMpParams certCs certRdmrAc = do
  let certVAddr = cmp'certVAddress certMpParams
  certIns <- genCertInputs certVAddr certCs certRdmrAc 100
  certRdmrIns <- genCertRdmrInputs certRdmrAc
  let certTokensToBurn = inv . fold $ [txOutValue certInOut | TxInInfo _ certInOut <- certIns]
      ctx = mkScriptContext (Minting certCs) (certIns <> certRdmrIns) [] certTokensToBurn [] []
  return $
    ctx
      { scriptContextTxInfo =
          (scriptContextTxInfo ctx)
            { txInfoValidRange = interval 101 201
            }
      }

genCorruptCertMpBurningCtx :: CertMpParams -> CurrencySymbol -> AssetClass -> Gen ScriptContext
genCorruptCertMpBurningCtx certMpParams certCs certRdmrAc = do
  let certVAddr = cmp'certVAddress certMpParams

  ctx <- genCorrectCertMpBurningCtx certMpParams certCs certRdmrAc

  -- Randomly pick a corruption
  ((mintAndPayOtherTokenNameAddr, removeCertRdmrInputs) :: (Bool, Bool)) <- arbitrary

  let corrupt =
        mkCorrupt
          [ (mintAndPayOtherTokenNameAddr, doMintAndPayOtherTokenNameAddr certCs certVAddr)
          , (removeCertRdmrInputs, doRemoveInputsWithToken certRdmrAc)
          ]

  -- If we didn't manage to corrupt anything, do it again
  let corruptedCtx = corrupt ctx
  if corruptedCtx == ctx
    then genCorruptCertMpBurningCtx certMpParams certCs certRdmrAc
    else return corruptedCtx

mkCorrupt :: forall {b}. [(Bool, b -> b)] -> b -> b
mkCorrupt = foldl' (\rest (b, act) -> if b then act . rest else rest) id

genCorrectAuthMpMintingCtx :: AuthMpParams -> CurrencySymbol -> Gen ScriptContext
genCorrectAuthMpMintingCtx authMpParams authCs = do
  let aaAc = amp'authAuthorityAc authMpParams
      aaQ = amp'requiredAtLeastAaQ authMpParams
  aaIns <- genAaInputs aaAc aaQ
  addr <- genAddress
  let authId = toBuiltin . hashTxInputs $ aaIns
      authTn = TokenName authId
      authToken = assetClassValue (assetClass authCs authTn) 1
      authOut = TxOut addr authToken NoOutputDatum Nothing
  return $
    mkScriptContext (Minting authCs) aaIns [] authToken [authOut] []

genCorruptAuthMpMintingCtx :: AuthMpParams -> CurrencySymbol -> Gen ScriptContext
genCorruptAuthMpMintingCtx authMpParams authCs = do
  ctx <- genCorrectAuthMpMintingCtx authMpParams authCs

  -- Randomly pick a corruption
  (mintAndPayOtherTokenName :: Bool) <- arbitrary

  let corrupt =
        mkCorrupt
          [ (mintAndPayOtherTokenName, doMintAndPayOtherTokenName authCs)
          ]

  -- If we didn't manage to corrupt anything, do it again
  let corruptedCtx = corrupt ctx
  if corruptedCtx == ctx
    then genCorruptAuthMpMintingCtx authMpParams authCs
    else return corruptedCtx

genCorrectAuthMpBurningCtx :: CurrencySymbol -> Gen ScriptContext
genCorrectAuthMpBurningCtx authCs = do
  authIns <- genAuthInputs authCs
  let authTokensToBurn = inv . fold $ [txOutValue authInOut | TxInInfo _ authInOut <- authIns]
  return $ mkScriptContext (Minting authCs) authIns [] authTokensToBurn [] []

genCorruptAuthMpBurningCtx :: CurrencySymbol -> Gen ScriptContext
genCorruptAuthMpBurningCtx authCs = do
  ctx <- genCorrectAuthMpBurningCtx authCs

  -- Randomly pick a corruption
  (mintAndPayOtherTokenNameAddr :: Bool) <- arbitrary

  otherAddr <- genAddress
  let corrupt =
        mkCorrupt
          [ (mintAndPayOtherTokenNameAddr, doMintAndPayOtherTokenNameAddr authCs otherAddr)
          ]

  -- If we didn't manage to corrupt anything, do it again
  let corruptedCtx = corrupt ctx
  if corruptedCtx == ctx
    then genCorruptAuthMpBurningCtx authCs
    else return corruptedCtx

genCorrectCertVSpendingCtx :: CurrencySymbol -> Address -> Gen ScriptContext
genCorrectCertVSpendingCtx certCs certVAddr = do
  certRdmrAc <- genCertRdmrAc
  certIns <- genCertInputs certVAddr certCs certRdmrAc 100
  let tokensToBurn = inv . fold $ [txOutValue inOut | TxInInfo _ inOut <- certIns]
  return $ mkScriptContext (Spending (txInInfoOutRef . head $ certIns)) certIns [] tokensToBurn [] []

genAddress :: Gen Address
genAddress = do
  scriptOrWallet :: Bool <- arbitrary
  if scriptOrWallet
    then do
      bs :: ByteString <- ByteString.pack <$> vectorOf 28 (arbitrary :: Gen Word8)
      return . scriptHashAddress . ValidatorHash . toBuiltin $ bs
    else do
      bs :: ByteString <- ByteString.pack <$> vectorOf 28 (arbitrary :: Gen Word8)
      return . pubKeyHashAddress . PubKeyHash . toBuiltin $ bs

genCurrencySymbol :: Gen CurrencySymbol
genCurrencySymbol = do
  bs <- stringToBuiltinByteString <$> vectorOf 28 (chooseEnum ('a', 'z'))
  return . CurrencySymbol $ bs

genAuthenticatonId :: Gen BuiltinByteString
genAuthenticatonId = do
  bs <- stringToBuiltinByteString <$> vectorOf 21 (chooseEnum ('a', 'z'))
  return $ "authid-" <> bs

-- | Distributes elements in the first argument over the elements of the second
distribute :: Ord a => [b] -> Set a -> Gen (Map a [b])
distribute total xs = do
  (leftover, distributed) <- distributeSingle total xs
  if null leftover
    then return distributed
    else do
      distributed' <- distribute leftover xs
      return $ Map.unionWith (<>) distributed distributed'

distributeSingle :: Ord a => [b] -> Set a -> Gen ([b], Map a [b])
distributeSingle total =
  foldM
    ( \(budget, dist) x ->
        if null budget
          then return (budget, Map.insert x [] dist)
          else do
            (taken, budget') <- take' budget []
            return (budget', Map.insert x taken dist)
    )
    (total, mempty)
  where
    take' [] outs = return (outs, [])
    take' (i : ins) outs = do
      b <- chooseAny
      if b
        then take' ins (i : outs)
        else return (outs, i : ins)

-- | Mutating functions to introduce corruptions

-- | Mints a token with a specified CurrencySymbol and 'other token name' and pays it to same output
doMintAndPayOtherTokenName :: CurrencySymbol -> ScriptContext -> ScriptContext
doMintAndPayOtherTokenName cs ctx =
  let ScriptContext txInfo _ = ctx
      otherAc = assetClass cs (TokenName "other token name")
   in ctx
        { scriptContextTxInfo =
            txInfo
              { txInfoMint = txInfoMint txInfo <> assetClassValue otherAc (toInteger . length . txInfoOutputs $ txInfo)
              , txInfoOutputs = txInfoOutputs txInfo <> [out {txOutValue = assetClassValue otherAc 1 <> txOutValue out} | out <- txInfoOutputs txInfo]
              }
        }

-- | Mints a token with a specified CurrencySymbol and 'other token name' and pays it a specified address
doMintAndPayOtherTokenNameAddr :: CurrencySymbol -> Address -> ScriptContext -> ScriptContext
doMintAndPayOtherTokenNameAddr cs addr ctx =
  let ScriptContext txInfo _ = ctx
      otherAc = assetClass cs (TokenName "other token name")
   in ctx
        { scriptContextTxInfo =
            txInfo
              { txInfoMint = txInfoMint txInfo <> assetClassValue otherAc 1
              , txInfoOutputs = txInfoOutputs txInfo <> [TxOut addr (assetClassValue otherAc 1) NoOutputDatum Nothing]
              }
        }

-- | Removes datums from all outputs
doRemoveOutputDatum :: ScriptContext -> ScriptContext
doRemoveOutputDatum ctx =
  let ScriptContext txInfo _ = ctx
   in ctx
        { scriptContextTxInfo =
            txInfo
              { txInfoOutputs = [out {txOutDatum = NoOutputDatum} | out <- txInfoOutputs txInfo]
              }
        }

-- | Replaces original address with some other address
doSendToOtherAddress :: Address -> ScriptContext -> ScriptContext
doSendToOtherAddress originalAddr ctx =
  let ScriptContext txInfo _ = ctx
      otherAddr = scriptHashAddress . ValidatorHash $ "other addr"
   in ctx
        { scriptContextTxInfo =
            txInfo
              { txInfoOutputs = [out {txOutAddress = otherAddr} | out <- txInfoOutputs txInfo, txOutAddress out == originalAddr]
              }
        }

-- | Replaces original address with some other address
doRemoveInputsWithToken :: AssetClass -> ScriptContext -> ScriptContext
doRemoveInputsWithToken ac ctx =
  let ScriptContext txInfo _ = ctx
   in ctx
        { scriptContextTxInfo =
            txInfo
              { txInfoInputs = [inp | inp@(TxInInfo _ inOut) <- txInfoInputs txInfo, assetClassValueOf (txOutValue inOut) ac == 0]
              }
        }

-- NOTE: That's why you want to use mlabs-haskell/plutus-simple-model
normalizeValue :: Value -> Value
normalizeValue v =
  Value . AssocMap.fromList . Map.toAscList . (AssocMap.fromList . Map.toAscList <$>) $
    Map.unionsWith
      (Map.unionWith (+))
      ( [ Map.singleton cs (Map.singleton tn q)
        | (cs, tokens) <- AssocMap.toList . getValue $ v
        , (tn, q) <- AssocMap.toList tokens
        ]
      )
