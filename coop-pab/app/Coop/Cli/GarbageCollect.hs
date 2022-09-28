module Coop.Cli.GarbageCollect (GarbageCollectOpts (..), garbageCollect) where

import BotPlutusInterface.Config (loadPABConfig)
import BotPlutusInterface.Types (PABConfig (pcOwnPubKeyHash))

import Coop.Pab (burnCerts)
import Coop.Pab.Aux (runBpi)
import Coop.Types (CoopDeployment)
import Data.Aeson (decodeFileStrict)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Plutus.V1.Ledger.Value (AssetClass)
import Plutus.V2.Ledger.Api (PubKeyHash)

data GarbageCollectOpts = GarbageCollectOpts
  { gco'pabConfig :: FilePath
  , gco'coopDeploymentFile :: FilePath
  , gco'certRdmrWalletPkh :: PubKeyHash
  , gco'certRdmrAc :: AssetClass
  }
  deriving stock (Show, Eq)

garbageCollect :: GarbageCollectOpts -> IO ()
garbageCollect opts = do
  coopDeployment <- fromMaybe (error "garbageCollect: Must have a CoopDeployment file in JSON") <$> decodeFileStrict @CoopDeployment (gco'coopDeploymentFile opts)
  pabConf <- either (\err -> error $ "garbageCollect: Must have a PABConfig file in Config format: " <> err) id <$> loadPABConfig (gco'pabConfig opts)

  (_, errOrAcs) <-
    runBpi @Text
      pabConf
        { pcOwnPubKeyHash = gco'certRdmrWalletPkh opts
        }
      $ burnCerts coopDeployment (gco'certRdmrAc opts)
  either
    (\err -> error $ "garbageCollect: " <> show err)
    (\_ -> putStrLn "garbageCollect: Collected $CERT UTxOs from @CertV using $CERT-RDMR tokens")
    errOrAcs
  return ()
