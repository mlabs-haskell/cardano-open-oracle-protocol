module Coop.Cli.GetState (GetStateOpts (..), getState) where

import BotPlutusInterface.Config (loadPABConfig)

import Coop.Pab qualified as Pab
import Coop.Pab.Aux (runBpi)
import Coop.Types (CoopDeployment)
import Data.Aeson (decodeFileStrict, encodeFile)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

data GetStateOpts = GetStateOpts
  { gco'pabConfig :: FilePath
  , gco'coopDeploymentFile :: FilePath
  , gco'coopStateFile :: FilePath
  }
  deriving stock (Show, Eq)

getState :: GetStateOpts -> IO ()
getState opts = do
  coopDeployment <- fromMaybe (error "getState: Must have a CoopDeployment file in JSON") <$> decodeFileStrict @CoopDeployment (gco'coopDeploymentFile opts)
  pabConf <- either (\err -> error $ "getState: Must have a PABConfig file in Config format: " <> err) id <$> loadPABConfig (gco'pabConfig opts)

  (_, errOrAcs) <-
    runBpi @Text
      pabConf
      $ Pab.getState coopDeployment
  either
    (\err -> error $ "getState: " <> show err)
    ( \coopState -> do
        putStrLn "getState: Success"
        encodeFile (gco'coopStateFile opts) coopState
    )
    errOrAcs
  return ()
