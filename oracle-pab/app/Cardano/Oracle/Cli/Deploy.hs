module Cardano.Oracle.Cli.Deploy (DeployOpts (..), DeployMode (..), deploy) where

data DeployMode = DEPLOY_PROD | DEPLOY_DEBUG deriving stock (Show, Read, Eq)

data DeployOpts = DeployOpts
  { do'Mode :: DeployMode
  , do'PlutusFile :: FilePath
  , do'DeploymentFile :: FilePath
  }
  deriving stock (Show, Eq)

deploy :: DeployOpts -> IO ()
deploy _ = do
  return ()
