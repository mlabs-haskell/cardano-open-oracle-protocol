{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Oracle.Cli.Compile (CompileOpts (..), CompileMode (..), compile) where

import Cardano.Oracle.Plutus (mkOneShotMintingPolicy, resourceMintingPolicy, resourceValidator)
import Cardano.Oracle.Types (CoopPlutus (CoopPlutus, cp'instanceMintingPolicy, cp'resourceMintingPolicy, cp'resourceValidator))
import Data.Aeson (encode)
import Data.ByteString.Lazy (writeFile)
import Plutarch (Config (Config), TracingMode (DoTracing, NoTracing))
import Plutarch qualified (compile)

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
