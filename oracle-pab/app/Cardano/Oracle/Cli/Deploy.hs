module Cardano.Oracle.Cli.Deploy (DeployOpts (..), deploy) where

import BotPlutusInterface.Config (loadPABConfig)
import Cardano.Oracle.Aux (DeployMode, loadCoopPlutus, runBpi)
import Cardano.Oracle.Pab qualified as Pab
import Data.Aeson (encodeFile)
import Data.Text (Text)

data DeployOpts = DeployOpts
  { do'Mode :: DeployMode
  , do'PabConfig :: FilePath
  , do'DeploymentFile :: FilePath
  }
  deriving stock (Show, Eq)

deploy :: DeployOpts -> IO ()
deploy opts = do
  coopPlutus <- loadCoopPlutus (do'Mode opts)
  pabConf <-
    either error id <$> loadPABConfig (do'PabConfig opts)
  (_, errOrCoopDeployment) <- runBpi @Text pabConf $ Pab.deploy @Text coopPlutus
  coopDeployment <- either (fail . show) pure errOrCoopDeployment
  encodeFile (do'DeploymentFile opts) coopDeployment
  return ()
