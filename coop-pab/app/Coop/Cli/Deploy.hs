module Coop.Cli.Deploy (DeployOpts (..), deploy) where

import BotPlutusInterface.Config (loadPABConfig)
import BotPlutusInterface.Types (PABConfig (pcOwnPubKeyHash))

import Coop.Pab qualified as Pab
import Coop.Pab.Aux (DeployMode, loadCoopPlutus, runBpi)
import Data.Aeson (encodeFile)
import Data.ByteString as BL (
  ByteString,
 )
import Data.Hex (unhex)
import Data.Text (Text)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash))
import Plutus.V2.Ledger.Api (PubKeyHash (PubKeyHash), toBuiltin)

data DeployOpts = DeployOpts
  { do'mode :: DeployMode
  , do'pabConfig :: FilePath
  , do'deploymentFile :: FilePath
  , do'godWalletPkh :: ByteString
  , do'aaWalletPkh :: ByteString
  , do'aaQ :: Integer
  }
  deriving stock (Show, Eq)

deploy :: DeployOpts -> IO ()
deploy opts = do
  coopPlutus <- loadCoopPlutus (do'mode opts)
  pabConf <-
    either error id <$> loadPABConfig (do'pabConfig opts)

  godWallet <- parsePkh $ do'godWalletPkh opts
  aaWallet <- parsePkh $ do'aaWalletPkh opts

  (_, errOrCoopDeployment) <-
    runBpi @Text
      pabConf
        { pcOwnPubKeyHash = unPaymentPubKeyHash godWallet
        }
      $ Pab.deployCoop @Text
        coopPlutus
        godWallet
        aaWallet
        (do'aaQ opts)
  coopDeployment <- either (fail . show) pure errOrCoopDeployment
  encodeFile (do'deploymentFile opts) coopDeployment
  return ()

parsePkh :: ByteString -> IO PaymentPubKeyHash
parsePkh pkh = do
  let pkhHashUnHex = unhex pkh
  pkhBytes <- case pkhHashUnHex of
    Left err -> do
      error err
    Right bs -> return bs
  return $ PaymentPubKeyHash . PubKeyHash . toBuiltin $ pkhBytes
