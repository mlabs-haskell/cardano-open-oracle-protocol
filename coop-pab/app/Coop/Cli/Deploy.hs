module Coop.Cli.Deploy (DeployOpts (..), deploy) where

import BotPlutusInterface.Config (loadPABConfig)
import BotPlutusInterface.Types (PABConfig (pcOwnPubKeyHash))

import Coop.Pab qualified as Pab
import Coop.Pab.Aux (DeployMode, loadCoopPlutus, runBpi)
import Data.Aeson (encodeFile)
import Data.Text (Text)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash))
import Plutus.V2.Ledger.Api (PubKeyHash)

data DeployOpts = DeployOpts
  { do'mode :: DeployMode
  , do'pabConfig :: FilePath
  , do'deploymentFile :: FilePath
  , do'godWalletPkh :: PubKeyHash
  , do'aaWalletPkh :: PubKeyHash
  , do'atLeastAaQRequired :: Integer
  , do'aaQToMint :: Integer
  }
  deriving stock (Show, Eq)

deploy :: DeployOpts -> IO ()
deploy opts = do
  coopPlutus <- loadCoopPlutus (do'mode opts)
  pabConf <-
    either error id <$> loadPABConfig (do'pabConfig opts)

  (_, errOrCoopDeployment) <-
    runBpi @Text
      pabConf
        { pcOwnPubKeyHash = do'godWalletPkh opts
        }
      $ Pab.deployCoop @Text
        coopPlutus
        (PaymentPubKeyHash $ do'aaWalletPkh opts)
        (do'atLeastAaQRequired opts)
        (do'aaQToMint opts)
  coopDeployment <- either (fail . show) pure errOrCoopDeployment
  encodeFile (do'deploymentFile opts) coopDeployment
  return ()
