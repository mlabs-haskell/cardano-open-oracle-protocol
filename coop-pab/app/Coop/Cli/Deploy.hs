module Coop.Cli.Deploy (DeployOpts (..), deploy) where

import BotPlutusInterface.Config (loadPABConfig)
import Coop.Pab qualified as Pab
import Coop.Pab.Aux (DeployMode, loadCoopPlutus, runBpi)
import Data.Aeson (encodeFile)
import Data.ByteString as BL (
  readFile,
 )
import Data.Hex (unhex)
import Data.Text (Text)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash))
import Plutus.V2.Ledger.Api (PubKeyHash (PubKeyHash), toBuiltin)

data DeployOpts = DeployOpts
  { do'mode :: DeployMode
  , do'pabConfig :: FilePath
  , do'deploymentFile :: FilePath
  , do'godWalletFile :: FilePath
  , do'aaWalletFile :: FilePath
  , do'aaQ :: Integer
  }
  deriving stock (Show, Eq)

deploy :: DeployOpts -> IO ()
deploy opts = do
  coopPlutus <- loadCoopPlutus (do'mode opts)
  pabConf <-
    either error id <$> loadPABConfig (do'pabConfig opts)

  godWallet <- readWallet $ do'godWalletFile opts
  aaWallet <- readWallet $ do'aaWalletFile opts

  (_, errOrCoopDeployment) <-
    runBpi @Text
      pabConf
      $ Pab.deployCoop @Text
        coopPlutus
        godWallet
        aaWallet
        (do'aaQ opts)
  coopDeployment <- either (fail . show) pure errOrCoopDeployment
  encodeFile (do'deploymentFile opts) coopDeployment
  return ()

readWallet :: FilePath -> IO PaymentPubKeyHash
readWallet ownPubKeyHashFp = do
  ownPubKeyHashUnHex <- unhex <$> BL.readFile ownPubKeyHashFp
  ownPubKeyHashBytes <- case ownPubKeyHashUnHex of
    Left err -> do
      error err
    Right bs -> return bs
  return $ PaymentPubKeyHash . PubKeyHash . toBuiltin $ ownPubKeyHashBytes
