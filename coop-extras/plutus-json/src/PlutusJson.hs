{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PlutusJson (plutusDataToJson, jsonToPlutusData) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key (fromText, toText)
import Data.Aeson.KeyMap (fromList, keys, toAscList)
import Data.Foldable (fold)
import Data.Int (Int64)
import Data.Scientific qualified as Sci
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Traversable (for)
import Data.Vector qualified as Vector
import PlutusTx (Data (B, Constr, I, List, Map), FromData (fromBuiltinData), ToData (toBuiltinData), builtinDataToData, dataToBuiltinData)

plutusDataToJson :: MonadFail m => Data -> m Aeson.Value
plutusDataToJson (Map elements) =
  Aeson.Object . fromList
    <$> for
      elements
      ( \(k, v) -> do
          k' <- plutusDataToObjectKey k
          v' <- plutusDataToJson v
          return (k', v')
      )
plutusDataToJson (List xs) = Aeson.Array . Vector.fromList <$> for xs plutusDataToJson
plutusDataToJson (B bs) = Aeson.String <$> (either (fail . show) return . decodeUtf8' $ bs)
plutusDataToJson (I i) = return (Aeson.Number (fromInteger i))
plutusDataToJson (Constr i fields) = case i of
  0 -> return (Aeson.Bool False)
  1 -> return (Aeson.Bool True)
  2 -> return Aeson.Null
  3 -> case fields of
    [I coeff, I exp] -> return (Aeson.Number $ Sci.scientific coeff (fromInteger exp))
    _ -> fail "PlutusData Constr with index 3 must be in Scientific form with fields [I coefficient, I base10exponent]"
  _ -> fail "PlutusData Constr indices must be either 0 - False | 1 - True | 2 - Null | 3 - Number"

objectKeyToPlutusData :: Aeson.Key -> Data
objectKeyToPlutusData = B . encodeUtf8 . toText

plutusDataToObjectKey :: MonadFail m => Data -> m Aeson.Key
plutusDataToObjectKey (B bs) = either (fail . show) (return . fromText) $ decodeUtf8' bs
plutusDataToObjectKey _ = fail "JSON Object key must be a UTF8 encoded bytestring"

jsonToPlutusData :: Aeson.Value -> Data
jsonToPlutusData (Aeson.Object obj) = Map [(objectKeyToPlutusData k, jsonToPlutusData v) | (k, v) <- toAscList obj]
jsonToPlutusData (Aeson.Array vec) = List (jsonToPlutusData <$> Vector.toList vec)
jsonToPlutusData (Aeson.String s) = B . encodeUtf8 $ s
jsonToPlutusData (Aeson.Number s) = case Sci.toBoundedInteger s of
  Just (n :: Int64) -> I (toInteger n)
  Nothing -> Constr 3 [I (Sci.coefficient s), I (toInteger $ Sci.base10Exponent s)]
jsonToPlutusData (Aeson.Bool b) = if b then Constr 1 [] else Constr 0 []
jsonToPlutusData Aeson.Null = Constr 2 []

instance ToData Aeson.Value where
  toBuiltinData = dataToBuiltinData . jsonToPlutusData

instance FromData Aeson.Value where
  fromBuiltinData = plutusDataToJson . builtinDataToData
