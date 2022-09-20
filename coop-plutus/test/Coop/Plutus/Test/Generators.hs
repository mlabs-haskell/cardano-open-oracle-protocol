module Coop.Plutus.Test.Generators (distribute, genCorruptCertMpMintingArgs, genAaInputs, genCorrectCertMpMintingArgs) where

import Test.QuickCheck (Arbitrary (arbitrary), Gen, choose, chooseAny)

import Control.Monad (foldM)
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
import PlutusLedgerApi.V2 (CurrencySymbol, Datum (Datum), LedgerBytes (LedgerBytes), OutputDatum (NoOutputDatum, OutputDatum), PubKeyHash (PubKeyHash), ScriptContext (ScriptContext, scriptContextTxInfo), ScriptPurpose (Minting), TxId (TxId, getTxId), TxInInfo (TxInInfo), TxInfo (TxInfo, txInfoDCert, txInfoData, txInfoFee, txInfoId, txInfoInputs, txInfoMint, txInfoOutputs, txInfoRedeemers, txInfoReferenceInputs, txInfoSignatories, txInfoValidRange, txInfoWdrl), TxOut (TxOut, txOutAddress, txOutDatum), TxOutRef (TxOutRef, txOutRefId, txOutRefIdx), ValidatorHash (ValidatorHash), Value, always, toBuiltin, toBuiltinData)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins.Class (stringToBuiltinByteString)

import Coop.Types (CertDatum (CertDatum), CertMpParams (cmp'authAuthorityAc, cmp'certVAddress, cmp'requiredAtLeastAaQ))
import Plutarch.Api.V2 (validatorHash)
import PlutusLedgerApi.V1.Interval (interval)

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
    , txInfoMint = mints
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

genCorrectCertMpMintingArgs :: CertMpParams -> CurrencySymbol -> Gen ScriptContext
genCorrectCertMpMintingArgs certMpParams certCs = do
  let aaAc = cmp'authAuthorityAc certMpParams
      aaQ = cmp'requiredAtLeastAaQ certMpParams
      certVAddr = cmp'certVAddress certMpParams
  aaIns <- genAaInputs aaAc aaQ
  let certId = toBuiltin . hashTxInputs $ aaIns
      certTn = TokenName certId
      certToken = assetClassValue (assetClass certCs certTn) 1
      certRdmrAc = assetClass (CurrencySymbol "$CERT-RDMR CS---------------") (TokenName "$CERT-RDMR TN")
      certDatum = CertDatum (LedgerBytes certId) (interval 0 100) certRdmrAc
      certOut = TxOut certVAddr certToken (OutputDatum . Datum . toBuiltinData $ certDatum) Nothing
  return $
    mkScriptContext (Minting certCs) aaIns [] certToken [certOut] []

genCorruptCertMpMintingArgs :: CertMpParams -> CurrencySymbol -> Gen ScriptContext
genCorruptCertMpMintingArgs certMpParams certCs = do
  let certVAddr = cmp'certVAddress certMpParams

  ctx <- genCorrectCertMpMintingArgs certMpParams certCs

  -- Randomly pick a corruption
  ((addOtherTokenName, removeOutputDatum, sendToOtherAddress) :: (Bool, Bool, Bool)) <- arbitrary

  let corruptions =
        mkCorruptions
          [ (addOtherTokenName, doAddOtherTokenName certVAddr)
          , (removeOutputDatum, doRemoveOutputDatum certVAddr)
          , (sendToOtherAddress, doSendToOtherAddress certVAddr)
          ]

  -- If we didn't manage to corrupt anything, do it again
  let corruptedCtx = corruptions ctx
  if corruptedCtx == ctx
    then genCorruptCertMpMintingArgs certMpParams certCs
    else return corruptedCtx
  where
    mkCorruptions = foldl' (\rest (b, act) -> if b then act . rest else rest) id

    -- Mints a $CERT token with 'other token name' and pays it to @CertV
    doAddOtherTokenName certVAddr ctx =
      let otherCertToken = assetClassValue (assetClass certCs (TokenName "other token name")) 1
          ScriptContext txInfo _ = ctx
       in ctx
            { scriptContextTxInfo =
                txInfo
                  { txInfoMint = txInfoMint txInfo <> otherCertToken
                  , txInfoOutputs = txInfoOutputs txInfo <> [TxOut certVAddr otherCertToken NoOutputDatum Nothing]
                  }
            }

    -- Removes datums from all @CertV outputs
    doRemoveOutputDatum certVAddr ctx =
      let ScriptContext txInfo _ = ctx
       in ctx
            { scriptContextTxInfo =
                txInfo
                  { txInfoOutputs = [out {txOutDatum = NoOutputDatum} | out <- txInfoOutputs txInfo, txOutAddress out == certVAddr]
                  }
            }

    doSendToOtherAddress certVAddr ctx =
      let otherAddr = scriptHashAddress . ValidatorHash $ "other addr"
          ScriptContext txInfo _ = ctx
       in ctx
            { scriptContextTxInfo =
                txInfo
                  { txInfoOutputs = [out {txOutAddress = otherAddr} | out <- txInfoOutputs txInfo, txOutAddress out == certVAddr]
                  }
            }

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
