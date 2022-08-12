{-# OPTIONS_GHC -Wno-orphans #-}

module Coop.Cli.Compile (CompileOpts (..), CompileMode (..), compile) where

import Coop.Plutus (mkSofMp, mkSofV)
import Coop.Plutus.Aux (mkOneShotMintingPolicy)
import Coop.Types (CoopPlutus (CoopPlutus, cp'mkCoopInstanceMp, cp'mkSofMp, cp'mkSofV))
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
  resMp <- either (\err -> fail $ "Failed compiling mkSofMp with " <> show err) pure (Plutarch.compile cfg mkSofMp)
  resV <- either (\err -> fail $ "Failed compiling mkSofV with " <> show err) pure (Plutarch.compile cfg mkSofV)

  let cs =
        CoopPlutus
          { cp'mkCoopInstanceMp = instMp
          , cp'mkSofMp = resMp
          , cp'mkSofV = resV
          }
  Data.ByteString.Lazy.writeFile (co'File opts) (encode cs)
  return ()
