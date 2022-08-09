module Cardano.Oracle.Cli.Deploy (DeployOpts (..), deploy) where

import BotPlutusInterface.Config (loadPABConfig)
import Cardano.Oracle.Aux (DeployMode, loadCoopPlutus, runBpi)
import Cardano.Oracle.Pab (createInstanceCs)
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
  runBpi @Text pabConf $ createInstanceCs @Text coopPlutus
  return ()
