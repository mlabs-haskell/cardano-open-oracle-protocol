{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Oracle.Cli.Compile (CompileOpts (..), CompileMode (..), compile) where

import Cardano.Oracle.Plutus (mkOneShotMintingPolicy, resourceMintingPolicy, resourceValidator)
import Codec.Serialise (deserialise, serialise)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), encode)
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString, fromStrict, toStrict)
import Data.ByteString.Base16 qualified as Base16S
import Data.ByteString.Lazy (writeFile)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Plutarch (Config (Config), TracingMode (DoTracing, NoTracing))
import Plutarch qualified (compile)
import PlutusLedgerApi.V1 (Script)

data CompileMode = COMPILE_PROD | COMPILE_DEBUG deriving stock (Show, Read, Eq)

data CompileOpts = CompileOpts
  { co'Mode :: CompileMode
  , co'File :: FilePath
  }
  deriving stock (Show, Eq)

compile :: CompileOpts -> IO ()
compile opts = do
  let cfg = case co'Mode opts of
        COMPILE_PROD -> Config NoTracing
        COMPILE_DEBUG -> Config DoTracing
  instMp <- either (\err -> fail $ "Failed compiling instanceMintingPolicy with " <> show err) pure (Plutarch.compile cfg mkOneShotMintingPolicy)
  resMp <- either (\err -> fail $ "Failed compiling resourceMintingPolicy with " <> show err) pure (Plutarch.compile cfg resourceMintingPolicy)
  resV <- either (\err -> fail $ "Failed compiling resourceValidator with " <> show err) pure (Plutarch.compile cfg resourceValidator)

  let cs =
        CoopPlutus
          { cp'instanceMintingPolicy = instMp
          , cp'resourceMintingPolicy = resMp
          , cp'resourceValidator = resV
          }
  Data.ByteString.Lazy.writeFile (co'File opts) (encode cs)
  return ()

-- TODO: Move this to coop-hs-types
data CoopPlutus = CoopPlutus
  { cp'instanceMintingPolicy :: Script
  , cp'resourceMintingPolicy :: Script
  , cp'resourceValidator :: Script
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CoopPlutus
instance FromJSON CoopPlutus

instance ToJSON Script where
  toJSON = toJSON . toStrict . serialise

instance ToJSON ByteString where
  toJSON = toJSON . decodeUtf8 . Base16S.encode

instance FromJSON Script where
  parseJSON json = deserialise . fromStrict <$> parseJSON json

instance FromJSON ByteString where
  parseJSON (Aeson.String text) = either fail pure (Base16S.decode . encodeUtf8 $ text)
  parseJSON invalid =
    prependFailure
      "parsing ByteString failed, "
      (typeMismatch "base16 encoded bytes" invalid)
