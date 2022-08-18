module Coop.Cli.Deploy (DeployOpts (..), deploy) where

import BotPlutusInterface.Config (loadPABConfig)
import Coop.Pab qualified as Pab
import Coop.Pab.Aux (DeployMode, loadCoopPlutus, runBpi)
import Data.Aeson (encodeFile)
import Data.Text (Text)

data DeployOpts = DeployOpts
  { do'mode :: DeployMode
  , do'pabConfig :: FilePath
  , do'deploymentFile :: FilePath
  }
  deriving stock (Show, Eq)

deploy :: DeployOpts -> IO ()
deploy opts = do
  coopPlutus <- loadCoopPlutus (do'mode opts)
  pabConf <-
    either error id <$> loadPABConfig (do'pabConfig opts)
  (_, errOrCoopDeployment) <- runBpi @Text pabConf $ Pab.deploy @Text coopPlutus
  coopDeployment <- either (fail . show) pure errOrCoopDeployment
  encodeFile (do'deploymentFile opts) coopDeployment
  return ()
