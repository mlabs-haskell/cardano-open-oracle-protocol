{-# OPTIONS_GHC -Wno-orphans #-}

module Coop.Cli.Compile (CompileOpts (..), CompileMode (..), compile) where

import Coop.Plutus (certV, fsV, mkAuthMp, mkCertMp, mkFsMp)
import Coop.Plutus.Aux (mkOneShotMp)
import Coop.Types (CoopPlutus (CoopPlutus, cp'certV, cp'fsV, cp'mkAuthMp, cp'mkCertMp, cp'mkFsMp, cp'mkOneShotMp))
import Data.Aeson (encode)
import Data.ByteString.Lazy (writeFile)
import Data.Kind (Type)
import Plutarch (Config (Config), TracingMode (DoTracing, NoTracing))
import Plutarch qualified (compile)

type CompileMode :: Type
data CompileMode = COMPILE_PROD | COMPILE_DEBUG deriving stock (Show, Read, Eq)

type CompileOpts :: Type
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
  mkOneShotMp' <- either (\err -> fail $ "Failed compiling mkOneShotMp with " <> show err) pure (Plutarch.compile cfg mkOneShotMp)
  mkAuthMp' <- either (\err -> fail $ "Failed compiling mkAuthMp with " <> show err) pure (Plutarch.compile cfg mkAuthMp)
  mkCertMp' <- either (\err -> fail $ "Failed compiling mkCertMp with " <> show err) pure (Plutarch.compile cfg mkCertMp)
  certV' <- either (\err -> fail $ "Failed compiling certV with " <> show err) pure (Plutarch.compile cfg certV)
  mkFsMp' <- either (\err -> fail $ "Failed compiling mkFsMp with " <> show err) pure (Plutarch.compile cfg mkFsMp)
  fsV' <- either (\err -> fail $ "Failed compiling fsV with " <> show err) pure (Plutarch.compile cfg fsV)

  let cs =
        CoopPlutus
          { cp'mkOneShotMp = mkOneShotMp'
          , cp'mkAuthMp = mkAuthMp'
          , cp'mkCertMp = mkCertMp'
          , cp'certV = certV'
          , cp'mkFsMp = mkFsMp'
          , cp'fsV = fsV'
          }
  Data.ByteString.Lazy.writeFile (co'File opts) (encode cs)
  return ()
