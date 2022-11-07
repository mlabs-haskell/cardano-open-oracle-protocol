{-# OPTIONS_GHC -Wno-orphans #-}

module Coop.ProtoAux (ProtoCardano (toCardano, fromCardano)) where

import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Hex (Hex (hex, unhex))
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Encoding qualified as Text
import Ledger qualified
import Lens.Micro ((.~), (^.))
import Plutus.V1.Ledger.Api (ToData (toBuiltinData), fromBuiltin, toBuiltin)
import PlutusTx (builtinDataToData, dataToBuiltinData)
import PlutusTx qualified
import Proto.Plutus qualified as Proto
import Proto.Plutus_Fields (base16, elements, extended, fields, finiteLedgerTime, idx, index, key, kvs, maybe'plutusData, transactionHash, value)
import Proto.Plutus_Fields qualified as PPlutus

class (MonadFail m) => ProtoCardano m proto cardano where
  toCardano :: proto -> m cardano
  fromCardano :: cardano -> m proto

instance MonadFail (Either Text) where
  fail = Left . Text.pack

-- | plutus-ledger-api types
instance (MonadFail m) => ProtoCardano m Proto.PubKeyHash Ledger.PubKeyHash where
  toCardano ppkh = Ledger.PubKeyHash . toBuiltin <$> fromHex (ppkh ^. base16)

  fromCardano (Ledger.PubKeyHash bytes) = return $ defMessage & base16 .~ (toHex . fromBuiltin $ bytes)

instance MonadFail m => ProtoCardano m Proto.ExtendedLedgerTime (Ledger.Extended Ledger.POSIXTime) where
  toCardano pext = case pext ^. extended of
    Proto.ExtendedLedgerTime'Extended'Unrecognized unrec -> fail (show unrec)
    Proto.ExtendedLedgerTime'NEG_INF -> return Ledger.NegInf
    Proto.ExtendedLedgerTime'POS_INF -> return Ledger.PosInf
    Proto.ExtendedLedgerTime'FINITE -> return (Ledger.Finite (Ledger.POSIXTime . toInteger $ pext ^. finiteLedgerTime))

  fromCardano cext = case cext of
    Ledger.NegInf -> return $ defMessage & extended .~ Proto.ExtendedLedgerTime'NEG_INF
    Ledger.Finite (Ledger.POSIXTime i) ->
      return $
        defMessage
          & extended .~ Proto.ExtendedLedgerTime'FINITE
          & finiteLedgerTime .~ fromInteger i
    Ledger.PosInf -> return $ defMessage & extended .~ Proto.ExtendedLedgerTime'POS_INF

instance (MonadFail m) => ProtoCardano m Proto.TxOutRef Ledger.TxOutRef where
  toCardano ptxOutRef = do
    txId <- toCardano (ptxOutRef ^. PPlutus.txId)
    return $ Ledger.TxOutRef txId (toInteger $ ptxOutRef ^. idx)

  fromCardano (Ledger.TxOutRef txId ix) = do
    txId' <- fromCardano txId
    return $
      defMessage
        & PPlutus.txId .~ txId'
        & idx .~ fromInteger ix

instance (MonadFail m) => ProtoCardano m Proto.TxId Ledger.TxId where
  toCardano ptxId = return $ Ledger.TxId (toBuiltin $ ptxId ^. transactionHash)
  fromCardano (Ledger.TxId bs) =
    return $
      defMessage & transactionHash .~ fromBuiltin bs

--  fromCardano (Ledger.PubKeyHash bytes) = return $ defMessage & base16 .~ (toHex . fromBuiltin $ bytes)

-- | PlutusData encoding
instance ToData Proto.PlutusList where
  toBuiltinData pl =
    dataToBuiltinData $
      PlutusTx.List [builtinDataToData . toBuiltinData $ el | el <- pl ^. elements]

instance ToData Proto.PlutusMap where
  toBuiltinData pm =
    dataToBuiltinData $
      PlutusTx.Map
        ( [ ( builtinDataToData . toBuiltinData $ kv ^. key
            , builtinDataToData . toBuiltinData $ kv ^. value
            )
          | kv <- pm ^. kvs
          ]
        )

instance ToData Proto.PlutusConstr where
  toBuiltinData pc =
    dataToBuiltinData $
      PlutusTx.Constr
        (toInteger $ pc ^. index)
        (builtinDataToData . toBuiltinData <$> pc ^. fields)

instance ToData Proto.PlutusData where
  toBuiltinData pd = case pd ^. maybe'plutusData of
    Nothing -> toBuiltinData (0 :: Integer)
    Just pd' -> case pd' of
      Proto.PlutusData'Pdint i -> dataToBuiltinData . PlutusTx.I . toInteger $ i
      Proto.PlutusData'Pdbytes bs -> dataToBuiltinData . PlutusTx.B $ bs
      Proto.PlutusData'Pdlist pl -> toBuiltinData pl
      Proto.PlutusData'Pdmap pm -> toBuiltinData pm
      Proto.PlutusData'Pdconstr pc -> toBuiltinData pc

-- | Helpers
toHex :: ByteString -> Text
toHex = decodeUtf8 . hex

fromHex :: MonadFail m => Text -> m ByteString
fromHex t = case unhex . Text.encodeUtf8 $ t of
  Left err -> fail err
  Right bytes -> return bytes
