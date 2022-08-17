{-# OPTIONS_GHC -Wno-orphans #-}

module Coop.Cli.Compile (CompileOpts (..), CompileMode (..), compile) where

import Coop.Plutus (mkFsMp, mkFsV)
import Coop.Plutus.Aux (mkOneShotMintingPolicy)
import Coop.Types (CoopPlutus (CoopPlutus, cp'mkCoopInstanceMp, cp'mkFsMp, cp'mkFsV))
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
  instMp <- either (\err -> fail $ "Failed compiling mkCoopInstanceMp with " <> show err) pure (Plutarch.compile cfg mkOneShotMintingPolicy)
  resMp <- either (\err -> fail $ "Failed compiling mkFsMp with " <> show err) pure (Plutarch.compile cfg mkFsMp)
  resV <- either (\err -> fail $ "Failed compiling mkFsV with " <> show err) pure (Plutarch.compile cfg mkFsV)

  let cs =
        CoopPlutus
          { cp'mkCoopInstanceMp = instMp
          , cp'mkFsMp = resMp
          , cp'mkFsV = resV
          }
  Data.ByteString.Lazy.writeFile (co'File opts) (encode cs)
  return ()
