module Coop.Cli.RedistributeAuth (RedistributeAuthOpts (..), redistributeAuth) where

import BotPlutusInterface.Config (loadPABConfig)
import BotPlutusInterface.Types (PABConfig (pcOwnPubKeyHash))

import Control.Lens (makeLenses, (^.))
import Coop.Pab (runRedistributeAuthsTrx)
import Coop.Pab.Aux (runBpi)
import Coop.Types (CoopDeployment)
import Data.Aeson (decodeFileStrict)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash))
import Plutus.V2.Ledger.Api (PubKeyHash)

data RedistributeAuthOpts = RedistributeAuthOpts
  { _pabConfig :: FilePath
  , _coopDeploymentFile :: FilePath
  , _howManyOutputs :: Int
  , _authWalletPkhs :: [PubKeyHash]
  }
  deriving stock (Show, Eq)

makeLenses ''RedistributeAuthOpts

redistributeAuth :: RedistributeAuthOpts -> IO ()
redistributeAuth opts = do
  coopDeployment <- fromMaybe (error "redistributeAuth: Must have a CoopDeployment file in JSON") <$> decodeFileStrict @CoopDeployment (opts ^. coopDeploymentFile)
  pabConf <- either (\err -> error $ "redistributeAuth: Must have a PABConfig file in Config format: " <> err) id <$> loadPABConfig (opts ^. pabConfig)

  for_
    (opts ^. authWalletPkhs)
    ( \authWallet -> do
        (_, errOrAcs) <-
          runBpi @Text
            pabConf
              { pcOwnPubKeyHash = authWallet
              }
            $ runRedistributeAuthsTrx
              coopDeployment
              (PaymentPubKeyHash authWallet)
              (opts ^. howManyOutputs)

        either
          (\err -> error $ "redistributeAuth: Failed redistributing output for Authenticator " <> show authWallet <> "with error " <> show err)
          ( \_ -> do
              putStrLn $ "redistributeAuth: Redistributed outputs for Authenticator " <> show authWallet
          )
          errOrAcs
    )
