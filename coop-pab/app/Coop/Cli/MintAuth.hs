module Coop.Cli.MintAuth (MintAuthOpts (..), mintAuth) where

import BotPlutusInterface.Config (loadPABConfig)
import BotPlutusInterface.Types (PABConfig (pcOwnPubKeyHash))

import Codec.Serialise (readFileDeserialise)
import Coop.Pab (mintAuthAndCert)
import Coop.Pab.Aux (runBpi)
import Coop.Types (CoopDeployment)
import Data.Aeson (decodeFileStrict)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Ledger (AssetClass, PaymentPubKeyHash (PaymentPubKeyHash))
import Plutus.V2.Ledger.Api (POSIXTime, PubKeyHash)

data MintAuthOpts = MintAuthOpts
  { mao'pabConfig :: FilePath
  , mao'coopDeploymentFile :: FilePath
  , mao'aaWalletPkh :: PubKeyHash
  , mao'certificateValidFrom :: POSIXTime
  , mao'certificateValidTo :: POSIXTime
  , mao'nAuthTokensPerWallet :: Integer
  , mao'certRdmrAcFile :: FilePath
  , mao'authWalletPkhs :: [PubKeyHash]
  }
  deriving stock (Show, Eq)

mintAuth :: MintAuthOpts -> IO ()
mintAuth opts = do
  coopDeployment <- fromMaybe (error "mintAuth: Must have a CoopDeployment file in JSON") <$> decodeFileStrict @CoopDeployment (mao'coopDeploymentFile opts)
  pabConf <- either (\err -> error $ "mintAuth: Must have a PABConfig file in Config format: " <> err) id <$> loadPABConfig (mao'pabConfig opts)
  certRdmrAc <- readFileDeserialise @AssetClass (mao'certRdmrAcFile opts)

  (_, errOrAcs) <-
    runBpi @Text
      pabConf
        { pcOwnPubKeyHash = mao'aaWalletPkh opts
        }
      $ mintAuthAndCert coopDeployment (PaymentPubKeyHash <$> mao'authWalletPkhs opts) (mao'nAuthTokensPerWallet opts) certRdmrAc (mao'certificateValidFrom opts) (mao'certificateValidTo opts)
  either
    (\err -> error $ "mintAuth: Must have $AUTH and $CERT asset classes" <> show err)
    ( \(certAc, authAc) -> do
        putStrLn $ "mintAuth: Minted $CERT " <> show certAc
        putStrLn $ "mintAuth: Minted $AUTH " <> show authAc
    )
    errOrAcs
  return ()
