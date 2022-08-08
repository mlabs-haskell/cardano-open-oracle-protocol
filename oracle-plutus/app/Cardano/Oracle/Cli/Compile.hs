module Cardano.Oracle.Cli.Compile (CompileOpts (..), CompileMode (..), compile) where

-- import Ply.Plutarch (writeTypedScript)

import Cardano.Oracle.Plutus (mkOneShotMintingPolicy, resourceMintingPolicy, resourceValidator)
import Plutarch (Config (Config), TracingMode (DoTracing, NoTracing))
import Ply.Plutarch.TypedWriter (typedWriterInfo)

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
      compiled = typedWriterInfo cfg
      osmp = compiled mkOneShotMintingPolicy
      rmp = typedWriterInfo cfg resourceMintingPolicy
      rv = typedWriterInfo cfg resourceValidator
  return ()
